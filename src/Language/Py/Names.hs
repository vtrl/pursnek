-- | Handles normalization of Python names
module Language.Py.Names where

import Prelude

import Data.List ( foldl' )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Text ( Text, isSuffixOf, replace )
import Language.PureScript.Names ( Ident, ModuleName(..) )
import qualified Language.PureScript.Names as PS


normalizeModuleName :: ModuleName -> ModuleName
normalizeModuleName (ModuleName m) = ModuleName $ normalizeModuleName_ m

normalizeModuleName_ :: Text -> Text
normalizeModuleName_ = normalizeNames

makeForeignModule :: ModuleName -> ModuleName
makeForeignModule (ModuleName m) = ModuleName (m <> "_foreign")

isForeignModule :: ModuleName -> Bool
isForeignModule (ModuleName m) = "_foreign" `isSuffixOf` m

runIdentPy :: Ident -> Text
runIdentPy = normalizeNames . PS.runIdent

normalizeNames :: Text -> Text
normalizeNames = renameKeywords . renameBuiltins . makeHuman
  where
  makeHuman = foldl' (.) id $ uncurry replace <$> humanEquivalent

  renameKeywords n
    | n `S.member` keywords = "_" <> n <> "_"
    | otherwise = n

  renameBuiltins n
    | n `S.member` builtins = "_" <> n <> "_"
    | otherwise = n

humanEquivalent :: [(Text, Text)]
humanEquivalent =
  [ ("$", "dollar")
  , ("'", "prime")
  , (".", "_")
  ]

keywords :: Set Text
keywords = S.fromAscList
  [ "False"
  , "None"
  , "True"
  , "and"
  , "as"
  , "assert"
  , "async"
  , "await"
  , "break"
  , "class"
  , "continue"
  , "def"
  , "del"
  , "elif"
  , "else"
  , "except"
  , "finally"
  , "for"
  , "from"
  , "global"
  , "if"
  , "import"
  , "in"
  , "is"
  , "lambda"
  , "nonlocal"
  , "not"
  , "or"
  , "pass"
  , "raise"
  , "return"
  , "try"
  , "while"
  , "with"
  , "yield"
  ]

builtins :: Set Text
builtins = S.fromAscList
  [ "ArithmeticError"
  , "AssertionError"
  , "AttributeError"
  , "BaseException"
  , "BlockingIOError"
  , "BrokenPipeError"
  , "BufferError"
  , "BytesWarning"
  , "ChildProcessError"
  , "ConnectionAbortedError"
  , "ConnectionError"
  , "ConnectionRefusedError"
  , "ConnectionResetError"
  , "DeprecationWarning"
  , "EOFError"
  , "Ellipsis"
  , "EnvironmentError"
  , "Exception"
  , "False"
  , "FileExistsError"
  , "FileNotFoundError"
  , "FloatingPointError"
  , "FutureWarning"
  , "GeneratorExit"
  , "IOError"
  , "ImportError"
  , "ImportWarning"
  , "IndentationError"
  , "IndexError"
  , "InterruptedError"
  , "IsADirectoryError"
  , "KeyError"
  , "KeyboardInterrupt"
  , "LookupError"
  , "MemoryError"
  , "ModuleNotFoundError"
  , "NameError"
  , "None"
  , "NotADirectoryError"
  , "NotImplemented"
  , "NotImplementedError"
  , "OSError"
  , "OverflowError"
  , "PendingDeprecationWarning"
  , "PermissionError"
  , "ProcessLookupError"
  , "RecursionError"
  , "ReferenceError"
  , "ResourceWarning"
  , "RuntimeError"
  , "RuntimeWarning"
  , "StopAsyncIteration"
  , "StopIteration"
  , "SyntaxError"
  , "SyntaxWarning"
  , "SystemError"
  , "SystemExit"
  , "TabError"
  , "TimeoutError"
  , "True"
  , "TypeError"
  , "UnboundLocalError"
  , "UnicodeDecodeError"
  , "UnicodeEncodeError"
  , "UnicodeError"
  , "UnicodeTranslateError"
  , "UnicodeWarning"
  , "UserWarning"
  , "ValueError"
  , "Warning"
  , "ZeroDivisionError"
  , "__build_class__"
  , "__debug__"
  , "__doc__"
  , "__import__"
  , "__loader__"
  , "__name__"
  , "__package__"
  , "__spec__"
  , "abs"
  , "all"
  , "any"
  , "ascii"
  , "bin"
  , "bool"
  , "breakpoint"
  , "bytearray"
  , "bytes"
  , "callable"
  , "chr"
  , "classmethod"
  , "compile"
  , "complex"
  , "copyright"
  , "credits"
  , "delattr"
  , "dict"
  , "dir"
  , "divmod"
  , "enumerate"
  , "eval"
  , "exec"
  , "exit"
  , "filter"
  , "float"
  , "format"
  , "frozenset"
  , "getattr"
  , "globals"
  , "hasattr"
  , "hash"
  , "help"
  , "hex"
  , "id"
  , "input"
  , "int"
  , "isinstance"
  , "issubclass"
  , "iter"
  , "len"
  , "license"
  , "list"
  , "locals"
  , "map"
  , "max"
  , "memoryview"
  , "min"
  , "next"
  , "object"
  , "oct"
  , "open"
  , "ord"
  , "pow"
  , "print"
  , "property"
  , "quit"
  , "range"
  , "repr"
  , "reversed"
  , "round"
  , "set"
  , "setattr"
  , "slice"
  , "sorted"
  , "staticmethod"
  , "str"
  , "sum"
  , "super"
  , "tuple"
  , "type"
  , "vars"
  , "zip"
  ]
