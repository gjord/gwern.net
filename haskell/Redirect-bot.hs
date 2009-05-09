{-
This is a program to generate variant capitalizations of strings which
are also the names of articles on the English Wikipedia. It will
then check to see whether the variants already exist as either redirects
or articles in their own right. If they don't, they will then
be created as redirects to the original article name. For performance
reasons, it lazily reads in from stdin using getContents;
hence you will need to do something like "cat list-of-article-names.txt | bot".
This program relies on the HSH (Haskell shell library), the
Pywikipediabot Python scripts, and also an up to date list of article
 titles which can be gotten from the Wikipedia database dumps at [[WP:DUMP]].
-}

module Main (main, testPages) where

import Permute (generateCaps) -- generateCaps provides all capitalization permutations w/r/t the first letters.
import HSH (run, runIO, cd)
import Monad (liftM, zipWithM_, when)

main :: IO ()
main = do
  cd "/home/gwern/bin/pywikipedia/" -- Let's hard-code file locations in only *one* place, OK?
  list <- liftM (\bs -> [n | n<-bs,  length n <= (2^(4::Int))]) $ liftM words getContents -- We assume that a list of whitespace-delimited article names for us to consume os being fed to us. Break into words, and filter out the too long ones.</nowiki>
  let replace a b = map (\x -> if a == x then b else x)
  mapM_ (match . replace '_' ' ') list  -- Simplify. Now match only has to deal with one article name at a time
  runIO "python pagefromfile.py -putthrottle:20 -safe -file:dict.txt -summary:'Bot edit: Creating a redirect from a miscapitalized name to appropriate page.\'"
  writeFile "dict.txt" "" -- Clean up.

match :: String -> IO ()
match article = zipWithM_ testPages x z
    where
      x = replicate (length z) article -- We must make a [article] equal in length to what z is; otherwise zip will truncate the list.
      z = generateCaps article -- long(er) list of possible capitalizations.

{- If page is not empty, exit. If it DNE, then tell pagefromfile to redirect 'redir' to 'article'.
This code is very dependent on pagefromfile.py's idiosyncratic method of operating!
Do not use unless you are sure pagefromfile.py is being used correctly here! -}
testPages :: String -> String -> IO ()
testPages article redir = do
  page <- catch (run ("python", ["get.py", redir]))
          (\_ -> return "") -- if errors, return "".
  when (page == "" || page == "\n") (appendFile "dict.txt" text)
      where text = "\n{{-start-}}\n" ++ "'''" ++ redir ++ "'''\n#REDIRECT [["  ++ article ++ "]]{{R from other capitalisation}} \n{{-stop-}}"
