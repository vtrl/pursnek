-- | Handles normalization of Python names
module Language.Py.Names where

import Data.Text ( Text, replace )
import Language.PureScript.Names ( ModuleName(..) )


normalizeModuleName :: ModuleName -> ModuleName
normalizeModuleName (ModuleName m) = ModuleName (replace "." "_" m)

normalizeModuleName_ :: Text -> Text
normalizeModuleName_ = replace "." "_"
