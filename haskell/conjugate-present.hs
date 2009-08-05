import System.Environment
import Data.List

main :: IO ()
main = do vs@(inf:pron:meaning:je:tu:il:nous:vous:ils:[]) <- getArgs
          let prefix = getPrefix $ drop 2 vs
          viceversa ("Define: " ++ inf ++ " (<i>" ++ pron ++ "</i>; v.)") meaning
          viceversa ("Je " ++ je) ("I " ++ meaning)
          viceversa ("Tu " ++ tu) ("You " ++ meaning)
          viceversa ("Il " ++ il) ("He " ++ meaning)
          viceversa ("Elle " ++ il) ("She " ++ meaning)
          viceversa ("On " ++ il) ("It " ++ meaning)
          viceversa ("Nous " ++ nous) ("We " ++ meaning)
          viceversa ("Vous " ++ vous) ("Y'all " ++ meaning)
          viceversa ("Ils " ++ ils) ("They <small>(m. pl)</small> " ++ meaning)
          viceversa ("Elles " ++ ils) ("They <small>(f. pl)</small> " ++ meaning)
          let foo' = foo meaning prefix
          foo' "I" "Je"  je 
          foo' "You" "Tu"  tu
          foo' "He" "Il" il
          foo' "She" "Elle" il
          foo' "It" "On" il
          foo' "We" "Nous" nous
          foo' "Y'all" "Vous" vous
          foo' "They <small>(m. pl)</small>" "Ils" ils
          foo' "They <small>(f. pl)</small>" "Elles" ils

foo m pre x y v = putStrLn $ "\"" ++ x ++ " " ++ m ++ ".\": " ++ y ++ " " ++ (pre ++ (replicate (length v - length pre) '_')) ++
                      ".\t" ++ y ++ " " ++ (pre ++ "<b>" ++ (v \\ pre) ++ "</b>.")

-- > getPrefix ["fooi", "foobar", "fooqux"] ~> "foo"
getPrefix :: [String] -> String
getPrefix v = let comm (x:xs) (y:ys) | x == y = x : comm xs ys; comm _ _ = [] in foldl1 comm v

viceversa :: String -> String -> IO ()
viceversa x y = do putStrLn $ x ++ ".\t" ++ y ++ "."
                   putStrLn $ y ++ ".\t" ++ x ++ "."