import Control.Monad (liftM)
import Data.List (intersperse)

main :: IO ()
main = do arg <- liftM (concatMap (permuteAndPutTogether . lines) . breakBlankLines) getContents
          mapM_ putStrLn arg

-- Utility function. It *should* be in the base libraries, but alas...
split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p s = let (l,s') = break p s in l : case s' of
                                           [] -> []
                                           (r:s'') -> [r] : split p s''

-- We don't want to split on newlines, but empty lines - there is one blank line
-- between each supplied item.
breakBlankLines :: String -> [String]
breakBlankLines = map (\x -> if head x == nl then tail x else x) . filter (not . (==) "") .
                   map concat . split (== "") . split (== nl)
                      where nl = '\n'

{- | OK. So each 'text' of our input has previously been split up by
 newlines. So if we wanted to memorize a quatrain or rubaiyat, say, it gets piped
 in on stdin as foo\nbar\n\quux\nbaz, turned into ["foo", "bar", "quux",
 "baz"], and then passed into 'permuteAndPutTogether'. This monster mangles
 the input through 'onHalves', puts the [[String]] pieces together
 (remembering that in the tab format, a literal newline is actual "<br>"), and
 then appends the answer, and catenates *that* (remembering that in the tabbed
 format, we separate questions and answers not with a newline, but a tab
 character). Finished, we let 'main' handle the I/O of printing to stdout. Whew!

 (In theory, this function should work on any length of text, thanks to the
 generality of 'onNths', but I haven't tested it with anything but the
 rubaiyats of Omar Khayyam.) -}
permuteAndPutTogether:: [String] -> [String]
permuteAndPutTogether str = map ((\x -> x ++ "\t" ++ concatQuestn str) . concatQuestn) $ onHalves hide str
           where concatQuestn = concat . intersperse "<br>"

{- | Take an answer, and hide it, so we can do Cloze deletion. ie, you could do
 question = [hide "First line", "Second line"]; answer = ["First line",
 "Second line"]. -}
hide :: String -> String
hide x = replicate (length x) '_'

{- | A highly general list function exploiting the capabilities of the List
 monad to generate permutations of a list such that we apply a function to all
 possible halves or thirds or nths.
 Thanks be to vixey on #haskell. -}
onNths :: (Num t) => t -> (a -> a) -> [a] -> [[a]]
onNths 0 _ list = return list
onNths n f (x:xs) = map (f x :) (onNths (n-1) f xs) ++ map (x :) (onNths n f xs)
onNths _ _ [] = [] -- Yes, we do need to put this definition last.

{- | Given a function f and the list [1,2], we'd get back [[f 1, 2], [1, f 2]];
 for f [1,2,3,4], we'd get [[f 1, f 2, 3, 4], [1, 2, f 3, f 4], [f 1, 2, f 3, 4], [1, f 2, 3, f 4]. -}
onHalves :: (a -> a) -> [a] -> [[a]]
onHalves f list = onNths (length list `div` 2) f list