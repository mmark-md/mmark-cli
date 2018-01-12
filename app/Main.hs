{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Main (main) where

import Control.Applicative
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Version (showVersion)
import Development.GitRev
import Options.Applicative hiding (ParseError)
import Paths_mmark_cli (version)
import System.Directory (makeAbsolute)
import System.Exit (exitFailure)
import Text.MMark (MMarkErr)
import Text.Megaparsec (ParseError)
import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text.IO                as T
import qualified Data.Text.Lazy              as TL
import qualified Lucid                       as L
import qualified Text.MMark                  as MMark

-- | Entry point of the program.

main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  (mdFileName, mdInput) <-
    case optInputFile of
      Nothing -> ("<stdin>",) <$> T.getContents
      Just file -> do
        absFile <- makeAbsolute file
        (absFile,) <$> T.readFile absFile
  case MMark.parse mdFileName mdInput of
    Left errs -> do
      if optJson
        then (BL.putStr . Aeson.encode . parseErrorsJson) errs
        else putStrLn (MMark.parseErrorsPretty mdInput errs)
      exitFailure
    Right doc -> do
      let htmlOutput = (TL.toStrict . L.renderText . MMark.render) doc
      if optJson
        then maybe BL.putStr BL.writeFile optOutputFile $
               Aeson.encode (htmlDocJson htmlOutput)
        else maybe T.putStr T.writeFile optOutputFile htmlOutput

----------------------------------------------------------------------------
-- Command line options parsing

-- | Command line options.

data Opts = Opts
  { optInputFile  :: !(Maybe FilePath)
    -- ^ File from which to read input (otherwise use stdin)
  , optOutputFile :: !(Maybe FilePath)
    -- ^ File to which to save output (otherwise use stdout)
  , optJson :: !Bool
    -- ^ Whether to output JSON
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (helper <*> ver <*> optsParser) . mconcat $
  [ fullDesc
  , progDesc "Command line interface to MMark markdown processor"
  , header   "mmark—command link interface to MMark markdown processor"
  ]
  where
    ver :: Parser (a -> a)
    ver = infoOption verStr . mconcat $
      [ long  "version"
      , short 'v'
      , help  "Print version of the program"
      ]
    verStr = intercalate "\n"
      [ unwords
        [ "mmark"
        , showVersion version
        , $gitBranch
        , $gitHash
        ]
      , "using mmark     (library) " ++ VERSION_mmark
      , "using mmark-ext (library) " ++ VERSION_mmark_ext
      ]

optsParser :: Parser Opts
optsParser = Opts
  <$> (optional . strOption . mconcat)
    [ long    "ifile"
    , short   'i'
    , metavar "IFILE"
    , help    "Read markdown source from this file"
    ]
  <*> (optional . strOption . mconcat)
    [ long    "ofile"
    , short   'o'
    , metavar "OFILE"
    , help    "Save rendered HTML document to this file"
    ]
  <*> (switch . mconcat)
    [ long    "json"
    , short   'j'
    , help    "Output results in JSON format"
    ]

----------------------------------------------------------------------------
-- Helpers

parseErrorsJson :: NonEmpty (ParseError Char MMarkErr) -> Aeson.Value
parseErrorsJson = undefined -- TODO

htmlDocJson :: Text -> Aeson.Value
htmlDocJson = undefined -- TODO
