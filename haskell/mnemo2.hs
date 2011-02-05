#!/usr/bin/env runhaskell

import Data.Char (isAlphaNum)

main :: IO ()
main = interact (unlines . tabify . couplet . lines)

couplet :: [String] -> [(String, String)]
couplet (a:b:[]) =  arrange a b
couplet (a:b:c) = arrange a b ++ couplet (b:c)
couplet _ = []

arrange :: String -> String -> [(String,String)]
arrange a b = [((erase a), b), (a,b)] ++ [(a, (erase b)), (a,b)]
 where erase :: String -> String
       erase = map (\x -> if isAlphaNum x then '_' else x )

tabify :: [(String, String)] -> [String]
tabify = map (\((x,y), (z,a)) -> x ++ "<br>" ++ y ++ "\t" ++ z ++ "<br>" ++ a) . tuplize
         where tuplize (a:b:c) = (a,b) : tuplize c
               tuplize _ = []