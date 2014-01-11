#! /usr/bin/env runhaskell
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- example usage: $ find ~/wiki/ -name "*.page" -exec runhaskell markdown-footnote-length {} \;
-- default: looks for footnotes >2400 characters long (too long on my screen)
import System.Environment (getArgs)
import Text.Pandoc (bottomUpM, def, nullMeta, readMarkdown, writeMarkdown,
                    Inline(Note), Pandoc(..))
import Control.Monad (void, when)

main :: IO ()
main = do (file:_) <- getArgs
          void (readFile file >>= processLint file)

-- 'drop 3' to avoid the near-infinite loop when files start with Hakyll metadata
processLint :: FilePath -> String -> IO Pandoc
processLint f x = bottomUpM (footNoteCheck f) (readMarkdown def (drop 3 x))

footNoteCheck :: FilePath -> Inline -> IO Inline
footNoteCheck f x@(Note cntnts) = do let md = writeMarkdown def (Pandoc nullMeta cntnts)
                                     when (length md > 2400) $ error (f ++ ": " ++ md)
                                     return x
footNoteCheck _ x = return x
