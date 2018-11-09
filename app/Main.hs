{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), Value (..))
import Data.List (intercalate)
import Data.Text (Text)
import Data.Version (showVersion)
import Data.Void
import Development.GitRev
import Options.Applicative hiding (ParseError)
import Paths_mmark_cli (version)
import System.Directory (makeAbsolute)
import System.Exit (exitFailure)
import Text.MMark (MMarkErr)
import Text.Megaparsec (Parsec, ParseErrorBundle (..), SourcePos (..))
import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy.Char8  as BL
import qualified Data.HashMap.Strict         as HM
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Data.Text.Lazy              as TL
import qualified Lucid                       as L
import qualified Text.MMark                  as MMark
import qualified Text.MMark.Extension.Common as Ext
import qualified Text.Megaparsec             as M
import qualified Text.Megaparsec.Char        as MC
import qualified Text.Megaparsec.Char.Lexer  as MCL
import qualified Text.Mustache               as U

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
    Left bundle -> do
      if optJson
        then (BL.putStrLn . Aeson.encode . parseErrorsJson) bundle
        else putStr (M.errorBundlePretty bundle)
      exitFailure
    Right doc -> do
      let exts = mconcat
            [ g optExtComment               Ext.commentParagraph
            , f optExtFontAwesome           Ext.fontAwesome
            , f optExtFootnotes             Ext.footnotes
            , f optExtKbd                   Ext.kbd
            , f optExtLinkTarget            Ext.linkTarget
            , f optExtMathJax               (Ext.mathJax (Just '$'))
            , g optExtObfuscateEmail        Ext.obfuscateEmail
            , f optExtPunctuationPrettifier Ext.punctuationPrettifier
#if __GLASGOW_HASKELL__ >= 804
            , f optExtGhcSyntaxHighlighter  Ext.ghcSyntaxHighlighter
#endif
            , f optExtSkylighting           Ext.skylighting
            , g optExtToc $ \(from,to) ->
               Ext.toc "toc" . MMark.runScanner doc . Ext.tocScanner $ \x ->
                 from <= x && x <= to
            ]
          f p x = if p then x else mempty
          g p x = maybe mempty x p
          applyTemplate tfile output = do
            t <- U.compileMustacheFile tfile
            return . TL.toStrict . U.renderMustache t $
              case MMark.projectYaml doc of
                Just (Object m) -> Object $
                  HM.insert "output" (String output) m
                _ -> Aeson.object
                  [ "output" .= output
                  ]
      htmlOutput <- maybe return applyTemplate optTemplate
        . TL.toStrict
        . L.renderText
        . MMark.render
        . MMark.useExtension exts
        $ doc
      if optJson
        then maybe BL.putStrLn BL.writeFile optOutputFile $
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
  , optTemplate :: !(Maybe FilePath)
    -- ^ Use the template located at this path

  , optExtComment :: !(Maybe Text)
    -- ^ Enable extension: 'Ext.commentParagraph'
  , optExtFontAwesome :: !Bool
    -- ^ Enable extension: 'Ext.fontAwesome'
  , optExtFootnotes :: !Bool
    -- ^ Enable extension: 'Ext.footnotes'
  , optExtKbd :: !Bool
    -- ^ Enable extension: 'Ext.kbd'
  , optExtLinkTarget :: !Bool
    -- ^ Enable extension: 'Ext.linkTarget'
  , optExtMathJax :: !Bool
    -- ^ Enable extension: 'Ext.mathJax'
  , optExtObfuscateEmail :: !(Maybe Text)
    -- ^ Enable extension: 'Ext.obfuscateEmail'
  , optExtPunctuationPrettifier :: !Bool
    -- ^ Enable extension: 'Ext.punctuationPrettifier'
#if __GLASGOW_HASKELL__ >= 804
  , optExtGhcSyntaxHighlighter :: !Bool
    -- ^ Enable extension: 'Ext.ghcSyntaxHighlighter'
#endif
  , optExtSkylighting :: !Bool
    -- ^ Enable extension: 'Ext.skylighting'
  , optExtToc :: !(Maybe (Int, Int))
    -- ^ Enable extension: 'Ext.toc'
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (helper <*> ver <*> optsParser) . mconcat $
  [ fullDesc
  , progDesc "Command line interface to the MMark markdown processor"
  , header   "mmark—command line interface to the MMark markdown processor"
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
    , help    "Read markdown source from this file (otherwise read from stdin)"
    ]
  <*> (optional . strOption . mconcat)
    [ long    "ofile"
    , short   'o'
    , metavar "OFILE"
    , help    "Save rendered HTML document to this file (otherwise write to stdout)"
    ]
  <*> (switch . mconcat)
    [ long    "json"
    , short   'j'
    , help    "Output parse errors and result in JSON format"
    ]
  <*> (optional . strOption . mconcat)
    [ long    "template"
    , short   't'
    , metavar "FILE"
    , help    "Use the template located at this path"
    ]
  <*> (optional . fmap T.pack . strOption . mconcat)
    [ long    "ext-comment"
    , metavar "PREFIX"
    , help    "Remove paragraphs that start with the given prefix"
    ]
  <*> (switch . mconcat)
    [ long    "ext-font-awesome"
    , help    "Enable support for inserting font awesome icons"
    ]
  <*> (switch . mconcat)
    [ long    "ext-footnotes"
    , help    "Enable support for footnotes"
    ]
  <*> (switch . mconcat)
    [ long    "ext-kbd"
    , help    "Enable support for wrapping things in kbd tags"
    ]
  <*> (switch . mconcat)
    [ long    "ext-link-target"
    , help    "Enable support for specifying link targets"
    ]
  <*> (switch . mconcat)
    [ long    "ext-mathjax"
    , help    "Enable support for MathJax formulas"
    ]
  <*> (optional . fmap T.pack . strOption . mconcat)
    [ long    "ext-obfuscate-email"
    , metavar "CLASS"
    , help    "Obfuscate email addresses assigning the specified class"
    ]
  <*> (switch . mconcat)
    [ long    "ext-punctuation"
    , help    "Enable punctuation prettifier"
    ]
#if __GLASGOW_HASKELL__ >= 804
  <*> (switch . mconcat)
    [ long    "ext-ghc-highlighter"
    , help    "Enable GHC syntax highlighter for Haskell code"
    ]
#endif
  <*> (switch . mconcat)
    [ long    "ext-skylighting"
    , help    "Enable syntax highlighting of code snippets with Skylighting"
    ]
  <*> (optional . option parseRange . mconcat)
    [ long    "ext-toc"
    , metavar "RANGE"
    , help    ("Enable generation of table of contents using the supplied "
      ++       "range of headers to include, e.g. \"1-6\" or \"2-4\"")
    ]

----------------------------------------------------------------------------
-- Helpers

-- | Represent the given collection of parse errors as a 'Value'.

parseErrorsJson :: ParseErrorBundle Text MMarkErr -> Value
parseErrorsJson ParseErrorBundle {..}
  = Aeson.toJSON
  . fmap parseErrorObj
  . fst
  $ M.attachSourcePos M.errorOffset bundleErrors bundlePosState
  where
    parseErrorObj :: (M.ParseError Text MMarkErr, SourcePos) -> Value
    parseErrorObj (err, SourcePos {..}) = Aeson.object
      [ "file"   .= sourceName
      , "line"   .= M.unPos sourceLine
      , "column" .= M.unPos sourceColumn
      , "text"   .= M.parseErrorTextPretty err
      ]

-- | Represent the given rendered HTML document as 'Aeson.Value'.

htmlDocJson :: Text -> Value
htmlDocJson html = Aeson.object
  [ "html" .= html
  ]

-- | Parse a range as two positive numbers separated by a hyphen.

parseRange :: ReadM (Int, Int)
parseRange = eitherReader $ \s ->
  case M.parse p "" s of
    Left bundle -> Left (M.errorBundlePretty bundle)
    Right x -> Right x
  where
    p :: Parsec Void String (Int, Int)
    p = do
      from <- MCL.decimal
      void (MC.char '-')
      to   <- MCL.decimal
      return (from, to)
