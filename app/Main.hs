{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Prelude

import Control.Monad ( when )
import Control.Monad.Trans.State ( evalStateT )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Foldable ( forM_ )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Language.Py.AST ( prettyPrintPy )
import Language.Py.CodeGen ( moduleToPy )
import Options.Applicative ( execParser )
import Pursnek.CLI ( Options(..), cli )
import System.Directory ( createDirectoryIfMissing, getCurrentDirectory )
import System.Exit ( exitFailure, exitSuccess )
import qualified System.FilePath as FP
import System.FilePath.Glob ( glob )
import System.IO ( hPutStrLn, stderr )

import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON
import Language.PureScript.Pretty.Common


moduleFromFile :: FilePath -> IO (Maybe (Module Ann))
moduleFromFile path = do
  json <- Aeson.decodeFileStrict' path
  pure $ json >>= fromJSON
  where
  fromJSON :: Aeson.Value -> Maybe (Module Ann)
  fromJSON = Aeson.parseMaybe (fmap snd) . moduleFromJSON


compileModule :: Module Ann -> Maybe Text
compileModule
  = fmap (Text.intercalate "\n")
  . flip evalStateT (PrinterState 0)
  . mapM (fmap runPlainString . prettyPrintPy)
  . flip moduleToPy Nothing


relativeToCurrent :: FilePath -> IO FilePath
relativeToCurrent p =
   if FP.isRelative p then do
     c <- getCurrentDirectory
     pure $ FP.joinPath [c, p]
   else
     pure p


compile :: Options -> IO ()
compile Options{..} = do
  psPath <- relativeToCurrent psOutput
  pyPath <- relativeToCurrent pyOutput

  files <- glob $ FP.joinPath [psPath, "**", "corefn.json"]

  when (null files) $ do
    putStrLn "No files found."
    exitSuccess

  createDirectoryIfMissing True pyPath

  forM_ files $ \file -> do
    mModule <- moduleFromFile file
    case mModule >>= compileModule of
      Just module_ -> do
        TIO.writeFile (FP.joinPath [pyPath, toModuleName file]) module_
      Nothing -> do
        hPutStrLn stderr $ "Invalid file: " <> file
        exitFailure
  where
  toModuleName
    = flip FP.addExtension ".py"
    . Text.unpack
    . Text.replace "." "_"
    . Text.pack
    . (!! 1)
    . reverse
    . FP.splitDirectories

main :: IO ()
main = execParser cli >>= compile
