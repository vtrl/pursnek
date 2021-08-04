{-# LANGUAGE OverloadedStrings #-}
-- | Type definitions for the Python AST.
module Language.Py.AST where

import Prelude

import Control.Arrow ( runKleisli, (<+>) )
import Control.Monad ( forM, mzero )
import Control.Monad.Trans.State ( StateT )
import Control.PatternArrows ( Operator, OperatorTable, Pattern )
import qualified Control.PatternArrows as PA
import Data.Text ( Text )
import qualified Data.Text as T
import Language.PureScript.PSString ( PSString, decodeString )
import Language.PureScript.Pretty.Common ( Emit, PrinterState(..) )
import qualified Language.PureScript.Pretty.Common as PT


-- | Built-in unary operators.
data UnaryOperator
  = Negate
  | Positive
  | Invert
  | Not
  deriving (Eq, Show)


-- | For use with is` and `in`.
data Negation
  = IsNegated
  | NotNegated
  deriving (Eq, Show)


-- | Built-in binary operators.
data BinaryOperator
  = Addition
  | Subtraction
  | Multiplication
  | Division
  | Modulo
  | MatrixMul
  | FloorDiv
  | ShiftLeft
  | ShiftRight
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | BooleanAnd
  | BooleanOr
  | LessThan
  | GreaterThan
  | EqualTo
  | NotEqualTo
  | LessThanOrEqualTo
  | GreaterThanOrEqualTo
  | Identity Negation
  | Membership Negation
  deriving (Eq, Show)


-- | Represents Python expressions.
data Py
  = PyNumericLiteral (Either Integer Double)
  | PyBooleanLiteral Bool
  | PyStringLiteral PSString
  | PyNoneLiteral
  | PyUnary UnaryOperator Py
  | PyBinary BinaryOperator Py Py
  | PyFunctionDef Text [Text] Py
  | PyFunctionApp Py [Py]
  | PyGetItem Py Py
  | PyAttribute Py Text
  | PyVariable Text
  | PyBlock [Py]
  | PyListLiteral [Py]
  | PyDictLiteral [(PSString, Py)]
  | PyAssignment Py Py
  | PyWhile Py Py
  | PyFor Text Py Py
  | PyIfElse Py Py (Maybe Py)
  | PyIfElif Py Py Py Py
  | PyReturn Py
  | PyRaise Py
  deriving (Eq, Show)


-- | Construct a unary operator.
unary_ :: forall e. Emit e => UnaryOperator -> (Py -> Text) -> Operator PrinterState Py e
unary_ operator toText = PA.Wrap (PA.mkPattern match) (<>)
  where
    match (PyUnary operator' expression)
      | operator == operator' = Just (PT.emit $ toText expression, expression)
    match _ = Nothing


-- | Construct a unary operator.
unary :: forall e. Emit e => UnaryOperator -> Text -> Operator PrinterState Py e
unary = (. const) . unary_


-- | Construct a binary operator.
binary :: forall e. Emit e => BinaryOperator -> Text -> Operator PrinterState Py e
binary operator text = PA.AssocL (PA.mkPattern match) combine
  where
    combine lhs rhs =
      lhs <> PT.emit (" " <> text <> " ") <> rhs

    match (PyBinary operator' lhs rhs)
      | operator == operator' = Just (lhs, rhs)
    match _ = Nothing


-- | Match against the `Py` data type.
literals :: forall e. Emit e => Pattern PrinterState Py e
literals = PA.mkPattern' match
  where
    emit' :: Text -> StateT PrinterState Maybe e
    emit' = pure . PT.emit

    runFold :: [StateT PrinterState Maybe e] -> StateT PrinterState Maybe e
    runFold = fmap mconcat . sequence

    match :: Py -> StateT PrinterState Maybe e
    match  PyNoneLiteral      = emit' "None"
    match (PyNumericLiteral  n) = emit' $ T.pack $ either show show n
    match (PyBooleanLiteral  b) = emit' $ if b then "True" else "False"
    match (PyStringLiteral   s) = emit' $ prettyPrintStringPy s
    match (PyVariable        v) = emit' v  -- TODO: ensure normalization
    match (PyListLiteral     l) = runFold
      [ emit' "["
      , do items <- mapM prettyPrintPy l
           pure $ PT.intercalate (PT.emit ", ") items
      , emit' "]"
      ]
    match (PyDictLiteral     l) = runFold
      [ emit' "{"
      , fmap mconcat $ forM l $ \(name, item) -> do
          runFold [ emit' $ prettyPrintStringPy name
                  , emit' ":"
                  , prettyPrintPy item
                  , emit' ","
                  ]
      , emit' "}"
      ]
    match (PyAssignment    n v) = runFold
      [ prettyPrintPy n
      , emit' " = "
      , prettyPrintPy v
      ]
    match (PyFunctionDef n a b) = runFold
      [ emit' "def "
      , emit' n
      , emit' ( "(" <> T.intercalate ", " a <> "):" )
      , prettyPrintPy b
      ]
    match (PyFunctionApp   f a) = runFold
      [ prettyPrintPy f
      , emit' "("
      , do args <- mapM prettyPrintPy a
           pure $ PT.intercalate (PT.emit ", ") args
      , emit' ")"
      ]
    match (PyGetItem      m i) = runFold
      [ prettyPrintPy m
      , emit' "["
      , prettyPrintPy i
      , emit' "]"
      ]
    match (PyAttribute    o n) = runFold
      [ prettyPrintPy o
      , emit' "."
      , emit' n
      ]
    match (PyReturn         p) = runFold
      [ emit' "return "
      , prettyPrintPy p
      ]
    match (PyRaise          p) = runFold
      [ emit' "raise "
      , prettyPrintPy p
      ]
    match (PyBlock          b) = runFold
      [ emit' "\n"
      , PT.withIndent $ do
          block <- mapM prettyPrintPy b
          prefix <- PT.currentIndent
          pure $ PT.intercalate (PT.emit "\n") $ map (prefix <>) block
      , emit' "\n"
      ]
    match (PyIfElse     c t e) = runFold
      [ emit' "if "
      , prettyPrintPy c
      , emit' ":"
      , prettyPrintPy t
      , maybe (pure mempty) (fmap (PT.emit "else:" <>) . prettyPrintPy) e
      ]
    match (PyIfElif   c t d e) = runFold
      [ emit' "if "
      , prettyPrintPy c
      , emit' ":"
      , prettyPrintPy t
      , emit' "elif "
      , prettyPrintPy d
      , emit' ":"
      , prettyPrintPy e
      ]
    match (PyWhile        c b) = runFold
      [ emit' "while "
      , prettyPrintPy c
      , emit' ":"
      , prettyPrintPy b
      ]
    match (PyFor        n r b) = runFold
      [ emit' $ "for " <> n <> " in "
      , prettyPrintPy r
      , emit' ":"
      , prettyPrintPy b
      ]
    match _ = mzero


-- | Pretty print the Python AST.
prettyPrintPy :: forall e. Emit e => Py -> StateT PrinterState Maybe e
prettyPrintPy = runKleisli $ PA.runPattern match
  where
    match :: Pattern PrinterState Py e
    match = PA.buildPrettyPrinter operatorTable (literals <+> fmap PT.parensPos match)

    operatorTable :: OperatorTable PrinterState Py e
    operatorTable =
      PA.OperatorTable
        [ [ unary Negate "-"
          , unary Positive "+"
          , unary Invert "~"
          , unary Not "not "
          ]
        , [ binary Multiplication "*"
          , binary Division "/"
          , binary FloorDiv "//"
          , binary Modulo "%"
          ]
        , [ binary Addition "+"
          , binary Subtraction "-"
          ]
        , [ binary ShiftLeft "<<"
          , binary ShiftRight ">>"
          ]
        , [ binary LessThan "<"
          , binary LessThanOrEqualTo "<="
          , binary GreaterThan ">"
          , binary GreaterThanOrEqualTo ">="
          ]
        , [ binary EqualTo "=="
          , binary NotEqualTo "!="
          ]
        , [ binary BitwiseAnd "&"
          ]
        , [ binary BitwiseOr "|"
          ]
        , [ binary BitwiseXor "^"
          ]
        , [ binary BooleanAnd "and"
          ]
        , [ binary BooleanOr "or"
          ]
        , [ binary (Identity NotNegated) "is"
          , binary (Identity IsNegated) "is not"
          ]
        , [ binary (Membership NotNegated) "in"
          , binary (Membership IsNegated) "not in"
          ]
        ]


-- | Pretty print a PureScript string to Python
prettyPrintStringPy :: PSString -> Text
prettyPrintStringPy purs =
  case decodeString purs of
    Just text_ -> "\"" <> T.concatMap encode text_ <> "\""
    _          -> undefined
  where
    encode :: Char -> Text
    encode '\0' = "\\x00"
    encode '\n' = "\\n"
    encode '\t' = "\\t"
    encode '\r' = "\\r"
    encode '\b' = "\\b"
    encode '\v' = "\\v"
    encode '\f' = "\\f"
    encode '"'  = "\\\""
    encode '\\' = "\\\\"
    encode c    = T.singleton c


-- | Recursively apply `f` into the AST.
everywhere :: (Py -> Py) -> Py -> Py
everywhere f = go
  where
    go :: Py -> Py
    go n@PyNoneLiteral        = f n
    go n@(PyNumericLiteral _) = f n
    go n@(PyBooleanLiteral _) = f n
    go n@(PyStringLiteral  _) = f n
    go n@(PyVariable       _) = f n
    go (PyUnary          o p) = f (PyUnary o (go p))
    go (PyBinary       o l r) = f (PyBinary o (go l) (go r))
    go (PyFunctionDef  n a b) = f (PyFunctionDef n a (go b))
    go (PyFunctionApp    p a) = f (PyFunctionApp (go p) (go <$> a))
    go (PyGetItem        p i) = f (PyGetItem (go p) (go i))
    go (PyAttribute      p n) = f (PyAttribute (go p) n)
    go (PyBlock            s) = f (PyBlock (go <$> s))
    go (PyListLiteral      i) = f (PyListLiteral (go <$> i))
    go (PyDictLiteral      i) = f (PyDictLiteral (fmap go <$> i))
    go (PyAssignment     p v) = f (PyAssignment (go p) (go v))
    go (PyWhile          c b) = f (PyWhile (go c) (go b))
    go (PyFor          n i b) = f (PyFor n (go i) (go b))
    go (PyIfElse       c t e) = f (PyIfElse (go c) (go t) (go <$> e))
    go (PyIfElif     c t d e) = f (PyIfElif (go c) (go t) (go d) (go e))
    go (PyReturn           p) = f (PyReturn (go p))
    go (PyRaise            p) = f (PyRaise (go p))
