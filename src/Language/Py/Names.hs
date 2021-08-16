-- | Handles normalization of Python names
module Language.Py.Names where

import Prelude

import Data.Text ( Text, isSuffixOf, replace )
import Language.PureScript.Names ( Ident, ModuleName(..) )
import qualified Language.PureScript.Names as PS


normalizeModuleName :: ModuleName -> ModuleName
normalizeModuleName (ModuleName m) = ModuleName (replace "." "_" m)

normalizeModuleName_ :: Text -> Text
normalizeModuleName_ = replace "." "_"

makeForeignModule :: ModuleName -> ModuleName
makeForeignModule (ModuleName m) = ModuleName (m <> "_foreign")

isForeignModule :: ModuleName -> Bool
isForeignModule (ModuleName m) = "_foreign" `isSuffixOf` m

runIdentPy :: Ident -> Text
runIdentPy = replace "'" "_" . replace "$" "_" . PS.runIdent
