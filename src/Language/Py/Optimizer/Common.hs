-- | Common utilities
module Language.Py.Optimizer.Common where

import Prelude

import Data.Text ( Text )
import Language.PureScript.Names ( ModuleName(..) )
import Language.Py.AST


onModule :: (Text, Text) -> (Py -> Bool)
onModule (moduleName, valueName) (PyVariable (Just (ModuleName m)) v)
  | moduleName == m && valueName == v = True
onModule (_, valueName) (PyVariable Nothing v)
  | valueName == v = True
onModule _ _ = False
