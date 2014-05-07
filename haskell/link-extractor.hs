import System.Environment (getArgs)
import Text.Pandoc (bottomUpM, def, readMarkdown, Inline(Link), Pandoc)

main :: IO ()
main = getArgs >>= mapM readFile >>= mapM_ analyzePage

analyzePage :: String -> IO Pandoc
analyzePage x = bottomUpM printLinks (readMarkdown def (unlines . drop 1 . lines $ x))

printLinks :: Inline -> IO Inline
printLinks (Link _ (x, _)) = putStrLn x >> return undefined
printLinks x                   = return x
