#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import Data.Maybe (fromMaybe, listToMaybe)

main :: IO ()
main = do delim <- fmap (head . fromMaybe "%" . listToMaybe) getArgs
          interact (unlines . concatMap (map tabify . generateQuestions . parseQuestion delim) . lines)

data Stuff = Question String | Answer String deriving (Eq, Show)

extract :: Stuff -> String
extract (Answer x) = x
extract (Question x) = x

getAnswer :: [Stuff] -> String
getAnswer xs = concat [x | Answer x <- xs]

tabify :: (String, String) -> String
tabify (q,a) = q ++ "\t" ++ a

-- parseQuestion '%' "David Hume was born %1711%." ~> [Question "David Hume was born ",Answer "1711"]
parseQuestion :: Char -> String -> [Stuff]
parseQuestion d = filter (/= Question "") . parseIntoStuff . split (==d)

-- Non-destructively break a list into sublists based on occurrence of an entry:
-- split (=='%') "Plato died in %328% BCE." ~> ["Plato died in ","%","328","%"," BCE."]
split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p s = let (l,s') = break p s in l : case s' of
                                           [] -> []
                                           (r:s'') -> [r] : split p s''

parseIntoStuff :: [String] -> [Stuff]
parseIntoStuff (x:[]) = [Question x]
parseIntoStuff (a:b:c:ds) | a == "%" && c == "%" = Answer b : parseIntoStuff ds
                          | otherwise = Question a : parseIntoStuff (b:c:ds)

generateQuestions :: [Stuff] -> [(String, String)]
generateQuestions x  = zip (baz x (queries x)) (cycle [getAnswer x])
    where queries :: [Stuff] -> [String]
          queries = drop 1 . mapM (:"_") . getAnswer
          baz :: [Stuff] -> [String] -> [String]
          baz ss = map (\q -> foo ss q)
          foo :: [Stuff] -> String -> String
          foo ss q = concatMap (bar q) ss
          bar :: String -> Stuff -> String
          bar q (Answer _) = q
          bar _ (Question a) = a