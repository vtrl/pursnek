{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE ViewPatterns #-}
-- | Handles transpilation from CoreFn to Py
module Language.Py.CodeGen where

import Prelude

import Control.Applicative
import Data.List ( foldl' )
import Data.Set ( Set )
import qualified Data.Set as S
import qualified Data.Text as T
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.PSString
import Language.Py.AST as Py
import Language.Py.Names
import Language.Py.Optimizer


moduleToPy :: Module Ann -> Maybe Py -> [Py]
moduleToPy (Module _ _ mn _ _ _ _ _ md) _ =
  let
    declarations = optimizeAll <$> foldMap (bindToPy PyAssignment) md
    usedModules = S.toList $ foldMap findModules declarations
  in
    (PyImport <$> usedModules) <> declarations
  where
  findModules :: Py -> Set ModuleName
  findModules = everything mappend go
    where
    go (PyVariable (Just m) _) = S.singleton m
    go _                       = mempty

  exprToPy :: Expr Ann -> Py
  exprToPy (Literal _ literal) = literalToPy literal
    where
    literalToPy :: Literal (Expr Ann) -> Py
    literalToPy (NumericLiteral n) = PyNumericLiteral n
    literalToPy (StringLiteral  s) = PyStringLiteral s
    literalToPy (CharLiteral    c) = PyStringLiteral (mkString . T.pack $ [c])
    literalToPy (BooleanLiteral b) = PyBooleanLiteral b
    literalToPy (ArrayLiteral   i) = PyListLiteral (exprToPy <$> i)
    literalToPy (ObjectLiteral  o) = PyDictLiteral (fmap exprToPy <$> o)

  exprToPy (Constructor _ t c a) =
    if not (null mr) then
      fn (PyDictLiteral (mt : mc : mr))
    else
      PyDictLiteral [mt, mc]
    where
    fn = foldl' (.) id (PyFunctionDef Nothing . pure <$> a')
    t' = runProperName t
    c' = runProperName c
    a' = runIdentPy <$> a
    mt = (,) (mkString "$type$") (PyStringLiteral (mkString t'))
    mc = (,) (mkString "$cons$") (PyStringLiteral (mkString c'))
    mr = liftA2 (,) mkString (PyVariable Nothing) <$> a'

  exprToPy (Accessor _ a e) = PyGetItem (exprToPy e) (PyStringLiteral a)

  exprToPy (ObjectUpdate _ e u) = PyBinary BitwiseOr (exprToPy e) (PyDictLiteral $ fmap exprToPy <$> u)

  exprToPy (Abs _ i e) = PyFunctionDef Nothing [runIdentPy i] (exprToPy e)

  exprToPy (App _ f e) = mkFunction f
    where
    mkFunction (Var (_, _, _, Just IsNewtype) _) = exprToPy e
    mkFunction _ = PyFunctionApp (exprToPy f) [exprToPy e]

  exprToPy (Var _ q) = qualifiedToPy q
    where
    qualifiedToPy (Qualified mn' i)
      | Just mn == mn' = PyVariable Nothing (runIdentPy i)
      | otherwise      = PyVariable (normalizeModuleName <$> mn') (runIdentPy i)

  exprToPy (Case _ e a) = chainTernaries . makeTernaries $ a
    where
      ex = exprToPy <$> e

      chainTernaries :: [(Py, Py)] -> Py
      chainTernaries = foldl' (flip id) PyNoneLiteral . map (uncurry PyTernary) . reverse

      makeTernaries :: [CaseAlternative Ann] -> [(Py, Py)]
      makeTernaries = concatMap makeTernary
        where
        makeTernary :: CaseAlternative Ann -> [(Py, Py)]
        makeTernary (CaseAlternative cb cg) =
          case unGuard cg of
            Left guards  -> guardToTernary <$> guards
            Right result -> [(resultModifiers result, conditionModifiers conditions)]
          where
          (conditions, resultModifiers, conditionModifiers) = joinSolved $ solveBinders ex cb

          unGuard :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> Either [(Py, Py)] Py
          unGuard (Right g) = Right (exprToPy g)
          unGuard (Left  g) = Left [(exprToPy r, exprToPy c) | (c, r) <- g]

          guardToTernary :: (Py, Py) -> (Py, Py)
          guardToTernary (r, c) = (resultModifiers r, conditionModifiers . PyBinary BooleanAnd conditions $ c)

          joinSolved :: [(Py, Py -> Py, Py -> Py)] -> (Py, Py -> Py, Py -> Py)
          joinSolved = (\(cn, rm, cm) -> (joinConditions cn, joinModifiers rm, joinModifiers cm)) . unzip3
            where
            joinModifiers :: [Py -> Py] -> (Py -> Py)
            joinModifiers = foldl' (.) id

            joinConditions :: [Py] -> Py
            joinConditions = foldl' (PyBinary BooleanAnd) (PyBooleanLiteral True)

          solveBinders :: [Py] -> [Binder Ann] -> [(Py, Py -> Py, Py -> Py)]
          solveBinders = zipWith solveBinder

          solveBinder :: Py -> Binder Ann -> (Py, Py -> Py, Py -> Py)
          solveBinder _ (NullBinder _) = (PyBooleanLiteral True, id, id)
          solveBinder v (LiteralBinder _ l) = matchLiteral l
            where
            matchLiteral :: Literal (Binder Ann) -> (Py, Py -> Py, Py -> Py)
            matchLiteral (NumericLiteral i) = (PyBinary EqualTo v (PyNumericLiteral i), id, id)
            matchLiteral (StringLiteral  s) = (PyBinary EqualTo v (PyStringLiteral s), id, id)
            matchLiteral (CharLiteral    c) = (PyBinary EqualTo v (PyStringLiteral (mkString . T.pack $ [c])), id, id)
            matchLiteral (BooleanLiteral b) = (PyBinary EqualTo v (PyBooleanLiteral b), id, id)
            matchLiteral (ArrayLiteral   r) = lengthCheck $ joinSolved $ solveBinders vIndexed r
              where
              vIndexed = PyGetItem v . PyNumericLiteral . Left <$> [0..]
              lengthCheck (cn, rm, cm) = (cn', rm, cm)
                where
                cn' =
                  PyBinary BooleanAnd
                  ( PyBinary EqualTo
                    ( PyFunctionApp (PyVariable Nothing "len") [v])
                    ( PyNumericLiteral (Left . toInteger . length $ r))
                  ) cn

            matchLiteral (ObjectLiteral  o) = joinSolved $ recurse <$> o
              where
              recurse = uncurry $ solveBinder . PyGetItem v . PyStringLiteral

          solveBinder v (VarBinder _ i) = (PyBooleanLiteral True, rename, rename)
            where
            rename = renameVariable i v

          solveBinder v (NamedBinder _ i b) = (cn, rename . rm, rename . cm)
            where
            rename = renameVariable i v
            (cn, rm, cm) = solveBinder v b

          solveBinder v (ConstructorBinder (_, _, _, Just (IsConstructor _ fields)) _ c b) = solution
            where
            solution = ctorCheck $ joinSolved $ zipWith recurse fields b
            recurse = solveBinder . PyGetItem v . PyStringLiteral . mkString . runIdentPy
            ctorCheck (cn, rm, cm) = (cn', rm, cm)
              where
              cn' =
                PyBinary BooleanAnd
                ( PyBinary EqualTo
                  ( PyGetItem v ( PyStringLiteral "$cons$" )
                  )
                  ( PyStringLiteral . mkString . runProperName . disqualify $ c
                  )
                ) cn

          solveBinder _ (ConstructorBinder (_, _, _, Nothing) _ _ _) = error "panic: malformed constructor"

          solveBinder _ _ = error "panic: unimplemented!"

          renameVariable :: Ident -> Py -> (Py -> Py)
          renameVariable i v = everywhere (\n -> if n == PyVariable Nothing (runIdentPy i) then v else n)

  exprToPy (Let _ b r) =
    PyFunctionApp
    ( PyParenthesize
      ( PyFunctionDef Nothing []
        ( PyGetItem
          ( PyListLiteral $ concatMap (bindToPy PyWalrus) b <> [exprToPy r]
          )
          ( PyNumericLiteral (Left (-1))
          )
        )
      )
    ) []

  bindToPy :: (Py -> Py -> Py) -> Bind Ann -> [Py]
  bindToPy f (NonRec _ i e) = pure $ nonRecToPy f i e
  bindToPy f (Rec e)        = (uncurry . uncurry $ const (nonRecToPy f)) <$> e

  nonRecToPy :: (Py -> Py -> Py) -> Ident -> Expr Ann -> Py
  nonRecToPy f i e@(extractAnn -> (_, c, _, _)) | not (null c) = nonRecToPy f i (modifyAnn removeComments e)
  nonRecToPy f i e = f (PyVariable Nothing $ runIdentPy i) (exprToPy e)
