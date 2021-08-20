module Pursnek.CLI where

import Prelude

import Options.Applicative



data Options = Options
  { psOutput :: FilePath
  , pyOutput :: FilePath
  , ffiPath  :: FilePath
  } deriving (Eq, Show)


options :: Parser Options
options = Options
  <$> ( option str (long "ps-output" <> short 'p' <> metavar "PATH")
     <|> pure "output"
      )
  <*> (option str (long "output" <> short 'o' <> metavar "PATH")
     <|> pure "py-output"
      )
  <*> (option str (long "ffi" <> metavar "PATH")
     <|> pure "py-ffi"
      )


cli :: ParserInfo Options
cli = info (options <**> helper) $
  fullDesc <> header "pursnek - Python backend for PureScript"
           <> footer "See also 'purs compile --help'"
