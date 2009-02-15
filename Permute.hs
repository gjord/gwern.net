module Permute (permutations, generateCaps, generateCapsSlow, generateCapsFast) where

import Data.Char (toUpper, toLower)
import Data.List (nub, isInfixOf, sort)
import Control.Monad (mapM)

-- Full blown permutation. This generates all regular permutations, with regard to position
permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations xs =
  [ y : zs | (y,ys) <- selections xs, zs <- permutations ys ]

selections :: [t] -> [(t, [t])]
selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]

{- Generate *all* permutations of a string w/r/t capitalizations. This does not permute
   in the sense of permutations.
   The nub removes all duplicate sequences. Some things cannot be capitalized. -}
permuteCap :: String -> [String]
permuteCap = nub . mapM (\ x -> [toUpper x, toLower x])

-- Given a list of strings, filter out everything that doesn't have them as a substring.
-- That is, filterInternal ["oo", "ar"] (permuteCap "foo bar") => ["Foo Bar","Foo bar","foo Bar","foo bar"]
filterInternal :: [String] -> [String] -> [String]
filterInternal c = filter (\s -> all (`isInfixOf` s) c)

-- 'filterInternal' is an improvement over doing it by hand, but we still need to generate the substrings.
-- We shall do it in a manner that means we select for capitalization variations only on initial letters.
generateCapsSlow :: String -> [String]
generateCapsSlow l = sort $ filterInternal (map tail $ words l) (permuteCap l)

{- In case you didn't notice, the previous algorithm used in 'permuteCap' generates *all* capitalizations, which is
   a O(n^2) operation! And since 'generateCapsSlow' is just filtering that list, that means that 'generateCapsSlow' is also
   O(n^2). This isn't good for titles of a reasonable length, so we instead we'll more directly generate things. It's more gnarly,
   but that's the price you pay for speed. This replacement algorithm is quicker. -}
generateListCaps :: String -> [[String]]
generateListCaps a = (\c d -> zipWith (zipWith (++)) (map (map (:[])) c) (repeat d)) y z
                      where y = mapM (\x -> [toUpper (head x), toLower (head x)]) (words a)
                            z = map tail (words a)

-- We'd rather return a list of Strings and not a list of lists of Strings,
-- so let's wrap unwords around generateListCaps's output.
generateCapsFast :: String -> [String]
generateCapsFast = map unwords . generateListCaps

-- What's our default? The quick one, of course.
generateCaps :: String -> [String]
generateCaps = generateCapsFast
