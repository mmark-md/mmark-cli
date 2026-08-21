{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Aeson (Value (..), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Version (showVersion)
import Data.Void
import Development.GitRev
import Lucid qualified as L
import Options.Applicative hiding (ParseError)
import Paths_mmark_cli (version)
import System.Directory (makeAbsolute)
import System.Exit (exitFailure)
import Text.MMark qualified as MMark
import Text.MMark.Extension.Common qualified as Ext
import Text.Megaparsec (ParseErrorBundle (..), Parsec, SourcePos (..))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL
import Text.Mustache qualified as U

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
  doc0 <- orDie optJson (MMark.parse mdFileName mdInput)
  let -- The order matters for the extensions that render code blocks: an
      -- extension is given a block before the ones that follow it and
      -- passes on what it does not recognize. Mermaid claims its own
      -- blocks, the highlighters colour the languages they know, and the
      -- line highlighter renders what is left over.
      renderExts =
        mconcat . concat $
          [ [Ext.commentParagraph prefix | Just prefix <- [optExtComment]],
            [Ext.footnotes | optExtFootnotes],
            [Ext.kbd | optExtKbd],
            [Ext.lazyImages | optExtLazyImages],
            [Ext.linkTarget | optExtLinkTarget],
            [Ext.mathJax (Just '$') | optExtMathJax],
            [Ext.permalinks | optExtPermalinks],
            [Ext.mermaid | optExtMermaid],
            [Ext.ghcSyntaxHighlighter | optExtGhcSyntaxHighlighter],
            [Ext.skylighting | optExtSkylighting],
            [Ext.lineHighlight | optExtLineHighlight]
          ]
      -- The table of contents is inserted before the transformations that
      -- rewrite text so that they apply to the table as well.
      trans =
        foldr (>=>) return . concat $
          [ [Ext.toc "toc" (tocOf from to) | Just (from, to) <- [optExtToc]],
            [Ext.emoji | optExtEmoji],
            [Ext.punctuationPrettifier | optExtPunctuationPrettifier]
          ]
      tocOf from to =
        MMark.runScanner (Ext.tocScanner (\x -> from <= x && x <= to)) doc0
  when optExtFootnotes
    $ orDie optJson
    $ MMark.runCheck
      (Ext.validateFootnotes (MMark.runScanner Ext.footnoteScanner doc0))
      doc0
  doc <- orDie optJson (MMark.runTrans trans doc0)
  let applyTemplate tfile output = do
        t <- U.compileMustacheFile tfile
        return . TL.toStrict . U.renderMustache t $
          case MMark.projectYaml doc of
            Just (Object m) ->
              Object $
                Aeson.KeyMap.insert "output" (String output) m
            _ ->
              Aeson.object
                [ "output" .= output
                ]
  htmlOutput <-
    maybe return applyTemplate optTemplate
      . TL.toStrict
      . L.renderText
      . MMark.render renderExts
      $ doc
  if optJson
    then
      maybe BL.putStrLn BL.writeFile optOutputFile $
        Aeson.encode (htmlDocJson htmlOutput)
    else maybe T.putStr T.writeFile optOutputFile htmlOutput

----------------------------------------------------------------------------
-- Command line options parsing

-- | Command line options.
data Opts = Opts
  { -- | File from which to read input (otherwise use stdin)
    optInputFile :: !(Maybe FilePath),
    -- | File to which to save output (otherwise use stdout)
    optOutputFile :: !(Maybe FilePath),
    -- | Whether to output JSON
    optJson :: !Bool,
    -- | Use the template located at this path
    optTemplate :: !(Maybe FilePath),
    -- | Enable extension: 'Ext.commentParagraph'
    optExtComment :: !(Maybe Text),
    -- | Enable extension: 'Ext.emoji'
    optExtEmoji :: !Bool,
    -- | Enable extension: 'Ext.footnotes'
    optExtFootnotes :: !Bool,
    -- | Enable extension: 'Ext.kbd'
    optExtKbd :: !Bool,
    -- | Enable extension: 'Ext.lazyImages'
    optExtLazyImages :: !Bool,
    -- | Enable extension: 'Ext.lineHighlight'
    optExtLineHighlight :: !Bool,
    -- | Enable extension: 'Ext.linkTarget'
    optExtLinkTarget :: !Bool,
    -- | Enable extension: 'Ext.mathJax'
    optExtMathJax :: !Bool,
    -- | Enable extension: 'Ext.mermaid'
    optExtMermaid :: !Bool,
    -- | Enable extension: 'Ext.permalinks'
    optExtPermalinks :: !Bool,
    -- | Enable extension: 'Ext.punctuationPrettifier'
    optExtPunctuationPrettifier :: !Bool,
    -- | Enable extension: 'Ext.ghcSyntaxHighlighter'
    optExtGhcSyntaxHighlighter :: !Bool,
    -- | Enable extension: 'Ext.skylighting'
    optExtSkylighting :: !Bool,
    -- | Enable extension: 'Ext.toc'
    optExtToc :: !(Maybe (Int, Int))
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> ver <*> optsParser) . mconcat $
    [ fullDesc,
      progDesc "Command line interface to the MMark markdown processor",
      header "mmark—command line interface to the MMark markdown processor"
    ]
  where
    ver :: Parser (a -> a)
    ver =
      infoOption verStr . mconcat $
        [ long "version",
          short 'v',
          help "Print version of the program"
        ]
    verStr =
      intercalate
        "\n"
        [ unwords
            [ "mmark",
              showVersion version,
              $gitBranch,
              $gitHash
            ],
          "using mmark     (library) " ++ VERSION_mmark,
          "using mmark-ext (library) " ++ VERSION_mmark_ext
        ]

optsParser :: Parser Opts
optsParser =
  Opts
    <$> (optional . strOption . mconcat)
      [ long "ifile",
        short 'i',
        metavar "IFILE",
        help "Read markdown source from this file (otherwise read from stdin)"
      ]
    <*> (optional . strOption . mconcat)
      [ long "ofile",
        short 'o',
        metavar "OFILE",
        help "Save rendered HTML document to this file (otherwise write to stdout)"
      ]
    <*> (switch . mconcat)
      [ long "json",
        short 'j',
        help "Output parse errors and result in JSON format"
      ]
    <*> (optional . strOption . mconcat)
      [ long "template",
        short 't',
        metavar "FILE",
        help "Use the template located at this path"
      ]
    <*> (optional . fmap T.pack . strOption . mconcat)
      [ long "ext-comment",
        metavar "PREFIX",
        help "Remove paragraphs that start with the given prefix"
      ]
    <*> (switch . mconcat)
      [ long "ext-emoji",
        help "Replace :shortcode: with the emoji it names"
      ]
    <*> (switch . mconcat)
      [ long "ext-footnotes",
        help "Enable support for footnotes"
      ]
    <*> (switch . mconcat)
      [ long "ext-kbd",
        help "Enable support for wrapping things in kbd tags"
      ]
    <*> (switch . mconcat)
      [ long "ext-lazy-images",
        help "Let the browser decide when to fetch each image"
      ]
    <*> (switch . mconcat)
      [ long "ext-line-highlight",
        help
          ( "Point at the lines of a code block named by its info string, "
              ++ "e.g. \"haskell {2,4-6}\""
          )
      ]
    <*> (switch . mconcat)
      [ long "ext-link-target",
        help "Enable support for specifying link targets"
      ]
    <*> (switch . mconcat)
      [ long "ext-mathjax",
        help "Enable support for MathJax formulas"
      ]
    <*> (switch . mconcat)
      [ long "ext-mermaid",
        help "Render mermaid code blocks as diagrams in the browser"
      ]
    <*> (switch . mconcat)
      [ long "ext-permalinks",
        help "Append a link to its own id to every heading"
      ]
    <*> (switch . mconcat)
      [ long "ext-punctuation",
        help "Enable punctuation prettifier"
      ]
    <*> (switch . mconcat)
      [ long "ext-ghc-highlighter",
        help "Enable GHC syntax highlighter for Haskell code"
      ]
    <*> (switch . mconcat)
      [ long "ext-skylighting",
        help "Enable syntax highlighting of code snippets with Skylighting"
      ]
    <*> (optional . option parseRange . mconcat)
      [ long "ext-toc",
        metavar "RANGE",
        help
          ( "Enable generation of table of contents using the supplied "
              ++ "range of headers to include, e.g. \"1-6\" or \"2-4\""
          )
      ]

----------------------------------------------------------------------------
-- Helpers

-- | Return the result, or print the errors that were produced instead of it
-- and exit. Both the parser and the extensions report their errors as a
-- 'ParseErrorBundle', so the two are presented in exactly the same way.
orDie ::
  (M.ShowErrorComponent e) =>
  -- | Whether to print the errors in the JSON format
  Bool ->
  -- | The result to return, or the errors to print
  Either (ParseErrorBundle Text e) a ->
  IO a
orDie json result =
  case result of
    Left bundle -> do
      if json
        then (BL.putStrLn . Aeson.encode . errorsJson) bundle
        else putStr (M.errorBundlePretty bundle)
      exitFailure
    Right x -> return x

-- | Represent the given collection of errors as a 'Value'.
errorsJson :: (M.ShowErrorComponent e) => ParseErrorBundle Text e -> Value
errorsJson ParseErrorBundle {..} =
  Aeson.toJSON
    . fmap errorObj
    . fst
    $ M.attachSourcePos M.errorOffset bundleErrors bundlePosState
  where
    errorObj (err, SourcePos {..}) =
      Aeson.object
        [ "file" .= sourceName,
          "line" .= M.unPos sourceLine,
          "column" .= M.unPos sourceColumn,
          "text" .= M.parseErrorTextPretty err
        ]

-- | Represent the given rendered HTML document as 'Aeson.Value'.
htmlDocJson :: Text -> Value
htmlDocJson html =
  Aeson.object
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
      to <- MCL.decimal
      return (from, to)
