{- | Useful for converting lists into multiple questions.
   For example, if we *really* wanted to memorize the order of the early Chinese
   dynasties, which "Shang -> Zhou -> Han", and we don't trust ourselves to
   write some basic questions to remember it, we could just do something like

> echo -e 'Shang\nZhou\nHan' | runhaskell memo4.hs

   and it will write 14 questions for us, which will make sure we remember it!
   The generated question are in tab-separated format (accepted by most SRS
   programs such as Mnemosyne or Anki) and look like this:

> What did Shang come before?\tZhou
> What succeeded Shang?\tZhou
> What ordinal position was Shang?\t1st
> What was 1st?\tShang
> What came before Zhou?\tShang
> What preceded Zhou?\tShang
> What did Zhou come before?\tHan
> What succeeded Zhou?\tHan
> What ordinal position was Zhou?\t2nd
> What was 2nd?\tZhou
> What came before Han?\tZhou
> What preceded Han?\tZhou
> What ordinal position was Han?\t3rd
> What was 3rd?\tHan

   If after these questions you can't remember the order instantly, then you
   need to consult a doctor! :)
-}
module Main (main) where
import Data.Maybe (fromJust)

{- -- The general design/idea. May not be isomorphic to the actual code...
x = ["Franklin", "Jefferson", "Adams"]
x' = zip [1..] x
x' == [(1, "Franklin"), (2, "Jefferson"), (3, "Adams")]

-- pair might seem like a lot of trouble, but now that each item has
-- a copy of all the data it needs (previous, next, and its logical position),
-- we can treat each item independently, and avoid things like zippers or folds
-- in favor of straightforward maps.

x'' = pair x'
x'' == [(Nothing, Just "Franklin", Just "Jefferson", Just 1),
        (Just "Franklin", Just "Jefferson", Just "Adams", Just 2),
        (Just "Jefferson", Just "Adams", Nothing, Just 3)]

x''' = clozeify x''
x''' == ["", -- Nothing preceded Franklin, so we fall through to "", which will
             -- get removed later by 'concat'
         "What came after Franklin?\tJefferson\nWhat succeeded Franklin?\tJefferson",
         "What ordinal position was Franklin?\t1st\nWhat was 1st?\tFranklin",
         "What came before Jefferson?\tFranklin\nWhat preceded Jefferson?\tFranklin",
         "What came after Jefferson?\tFranklin\nWhat succeeded Jefferson?\tAdams",
         "What ordinal position was Jefferson\t2nd\nWhat was 2nd?\tJefferson",
         "What came before Adams?\tJefferson\nWhat preceded Adams?\tJefferson",
         "", -- Nothing succeeded Adams in our list
         "What ordinal position was Adams?\t3rd\nWhat was 3rd?\tAdams"]
x'''' = unlines x'''
x'''' == "What came after Franklin?\tJefferson\nWhat succeeded Franklin?\tJefferson\nWhat ordinal position was Franklin?\t1st\nWhat was 1st?\tFranklin\mWhat came before Jefferson?\tFranklin\nWhat preceded Jefferson?\tFranklin\nWhat came after Jefferson?\tFranklin\nWhat succeeded Jefferson?\tAdams\nWhat ordinal position was Jefferson\t2nd\nWhat was 2nd?\tJefferson\nWhat came before Adams?\tJefferson\nWhat preceded Adams?\tJefferson\nWhat ordinal position was Adams?\t3rd\nWhat was 3rd?\tAdams"

-- Note that this assumes one has concatted the [[String]] result of clozeify,
-- and optionally gotten rid of the various "" in it.
-- But now we can do what we want with it, like print it.

main = do list <- fmap lines getContents
          let foo = concat . clozeify . pair . number $ list
          print $ unlines foo

-- But we can rewrite this using 'interact'.
-}

main :: IO ()
main = interact (unlines . filter (/= "") . concat . clozeify . pair . number . lines)

number :: [String] -> [(Int, String)]
number = zip [1..]

type Answers = (Maybe String, -- ^ Previous entry in list
                Maybe String, -- ^ Current entry in list
                Maybe String, -- ^ Next entry in list
                Maybe Int) -- ^ Index from original list of items

pair :: [(Int, String)] -> [Answers]
pair x = map bar x
    where bar = \y -> let index = fst y in (lookup (index - 1) x, -- we  look up the previous entry
                       Just $ snd y, -- our current item, eg "Franklin"; this is guaranteed to be there, so we Just it
                       lookup (index + 1) x,
                       Just $ fst y)

clozeify :: [Answers] -> [[String]]
clozeify = map (\(a, b, c, d) -> let b' = fromJust b in [(case a of
                                  Nothing -> ""
                                  Just a' -> "What came before " ++ b' ++ "?\t" ++ a' ++
                                            "\n" ++
                                            "What preceded " ++ b' ++ "?\t" ++ a'),
                               (case c of
                                  Nothing -> ""
                                  Just c' -> "What did " ++ b' ++ " come before?\t" ++ c' ++
                                            "\n" ++
                                            "What succeeded " ++ b' ++ "?\t" ++ c'),
                               (case d of
                                  Nothing -> ""
                                  Just d' -> "What ordinal position was " ++ b' ++ "?\t" ++ englishfy d' ++
                                            "\n" ++
                                            "What was " ++ englishfy d' ++ "?\t" ++ b')])

englishfy :: Int -> String
englishfy n
   | c (1::Int) = n' ++ "st" -- 21st, 1st, etc.
   | c (2::Int) = n' ++ "nd"
   | c (3::Int) = n' ++ "rd"
   | otherwise = n' ++ "th" -- 20th, 24th, 25th, 26th, 27th, 28th, 29th etc.
 where n' = show n
       c x = mod n 10 == x -- it's the terminal digit that determines the English treatment
