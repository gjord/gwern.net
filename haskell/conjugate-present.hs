import System.Environment (getArgs)
import Data.List ((\\))

type PronounConj = (String, -- ^ basic French pronoun eg. "Je"
                    String, -- ^ the translation of previous; eg. "I"
                    String) -- ^ the selected verb's conjugation, eg. for avoir "ai"
main :: IO ()
main = do vs <- getArgs
          case vs of 
           (inf:pronunc:meaning:_je:_tu:_il:_nous:_vous:_ils:[]) -> do
               let prefix = getPrefix $ drop 2 vs
               let allpron = zip3 ["Je", "Tu", "Il", "Elle", "On", "Nous", "Vous", "Ils", "Elles"]
                          ["I", "You", "He", "She", "It", "We", "Y'all", 
                           "They <small>(m. pl)</small> ", "They <small>(m. pl)</small> "] 
                          vs
               viceversa ("Define: " ++ inf ++ " (<i>" ++ pronunc ++ "</i>; v.)") meaning
               mapM_ (viceversa3 meaning) allpron
               let cloze' = clozeDelete meaning prefix -- since it's same verb and prefix for all of the conjugations
               mapM_ cloze' allpron
           -- fallback if the input isn't *exactly* right
           _ -> putStrLn  "Example usage:\n$ conjugate-present avoir '/avwaʀ/' 'have/possess' ai as a avons allez ont"

clozeDelete :: String -> String -> PronounConj -> IO ()
clozeDelete m pre (y,x,v) = putStrLn $ "\"" ++ x ++ " " ++ m ++ ".\": " ++ y ++ " " ++ (pre ++ (replicate (length v - length pre) '_')) ++
                      ".\t" ++ y ++ " " ++ (pre ++ "<b>" ++ (v \\ pre) ++ "</b>.")

-- > getPrefix ["fooi", "foobar", "fooqux"] ~> "foo"
getPrefix :: [String] -> String
getPrefix v = let comm (x:xs) (y:ys) | x == y = x : comm xs ys; comm _ _ = [] in foldl1 comm v

-- Print out a question/answer, and then answer/question - good for definitions
viceversa :: String -> String -> IO ()
viceversa x y = do putStrLn $ x ++ ".\t" ++ y ++ "."
                   putStrLn $ y ++ ".\t" ++ x ++ "."

-- abstract out repetition like 
-- > viceversa ("Je " ++ je) ("I " ++ meaning)..."Ils" ++ ils
viceversa3 :: String -> PronounConj -> IO ()
viceversa3 meaning (fr,en,conj) = viceversa (fr ++ " " ++ conj) (en ++ " " ++ meaning)