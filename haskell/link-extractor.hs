import System.Environment (getArgs)
import Text.Pandoc (defaultParserState, processWithM, readMarkdown, Inline(Link), Pandoc)

main :: IO ()
main = getArgs >>= mapM readFile >>= mapM_ analyzePage
          
analyzePage :: String -> IO Pandoc
analyzePage x = processWithM printLinks (readMarkdown defaultParserState x)

printLinks :: Inline -> IO Inline
printLinks (Link _ (x, _)) = putStrLn x >> return undefined
printLinks x                   = return x
