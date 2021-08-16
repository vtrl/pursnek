-- | Handles optimizations of the Python AST
module Language.Py.Optimizer
  ( optimizeAll
  , module Optimizer
  ) where

import Language.Py.AST ( Py )
import Language.Py.Optimizer.Inliner as Optimizer


optimizeAll :: Py -> Py
optimizeAll = optimizeInline
