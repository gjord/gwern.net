#! /usr/bin/env runhaskell
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- example usage: $ find ~/wiki/ -name "*.page" -exec runhaskell markdown-footnote-length {} \;
-- default: looks for footnotes >2400 characters long (too long on my screen)
import System.Environment (getArgs)
import Text.Pandoc (bottomUpM, defaultParserState, readMarkdown, writeMarkdown,
                    CiteMethod(Citeproc), HTMLSlideVariant(NoSlides), HTMLMathMethod(PlainMath),
                    Inline(Note), Meta(..), Pandoc(..), WriterOptions(..))
import Control.Monad (void, when)
import Text.Pandoc.Highlighting (pygments)

main :: IO ()
main = do (file:_) <- getArgs
          void (readFile file >>= processLint file)

-- 'drop 3' to avoid the near-infinite loop when files start with Hakyll metadata
processLint :: FilePath -> String -> IO Pandoc
processLint f x = bottomUpM (footNoteCheck f) (readMarkdown defaultParserState (drop 3 x))

footNoteCheck :: FilePath -> Inline -> IO Inline
footNoteCheck f x@(Note cntnts) = do let md = writeMarkdown def (Pandoc nullMeta cntnts)
                                     when (length md > 2400) $ error (f ++ ": " ++ md)
                                     return x
footNoteCheck _ x = return x

nullMeta :: Meta
nullMeta = Meta { docTitle = []
                , docAuthors = []
                , docDate = [] }

def :: WriterOptions
def = WriterOptions { writerStandalone         = False
                      , writerTemplate         = ""
                      , writerVariables        = []
                      , writerTabStop          = 4
                      , writerTableOfContents  = False
                      , writerSlideVariant     = NoSlides
                      , writerIncremental      = False
                      , writerHTMLMathMethod   = PlainMath
                      , writerIgnoreNotes      = False
                      , writerNumberSections   = False
                      , writerSectionDivs      = False
                      , writerReferenceLinks   = False
                      , writerWrapText         = True
                      , writerColumns          = 72
                      , writerIdentifierPrefix = ""
                      , writerSourceDirectory  = "."
                      , writerUserDataDir      = Nothing
                      , writerCiteMethod       = Citeproc
                      , writerBiblioFiles      = []
                      , writerHtml5            = False
                      , writerBeamer           = False
                      , writerSlideLevel       = Nothing
                      , writerChapters         = False
                      , writerListings         = False
                      , writerHighlight        = False
                      , writerSetextHeaders    = True
                      , writerTeXLigatures     = True
                      , writerEPUBMetadata     = ""
                      , writerStrictMarkdown   = True
                      , writerLiterateHaskell  = False
                      , writerEmailObfuscation = undefined
                      , writerHighlightStyle   = pygments
                      , writerXeTeX = False
                      }
