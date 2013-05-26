#! /usr/bin/env runhaskell
-- example usage: $ find ~/wiki/ -name "*.page" -exec runhaskell length-warner.hs {} \;
-- default: looks for lines >110 characters long
import System.Environment (getArgs)
import Text.Pandoc (bottomUpM, defaultParserState, readMarkdown, Block(CodeBlock), Pandoc)
import Control.Monad (void, when)

main :: IO ()
main = do (file:_) <- getArgs
          void (readFile file >>= processLint file)

-- 'drop 3' to avoid the near-infinite loop when files start with Hakyll metadata
processLint :: FilePath -> String -> IO Pandoc
processLint f x = bottomUpM (lineCheck f) (readMarkdown defaultParserState (drop 3 x))

lineCheck :: FilePath -> Block -> IO Block
lineCheck f x@(CodeBlock _ cntnts) = do mapM_ (\a -> when (length a >= 110)
                                                  (error $ f++": "++a)) (lines cntnts)
                                        return x
lineCheck _ x = return x
