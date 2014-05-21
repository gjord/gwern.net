#! /usr/bin/env runhaskell
-- $ cabal install cassava stringsearch
-- $ wget http://www.gwern.net/docs/gwern-goodreads.csv
-- # Demonstration usage:
-- $ cd ~/wiki/ && ./haskell/goodreadsToMarkdown.hs docs/gwern-goodreads.csv > book-reviews.page

{- Background on parsing the GoodReads CSV export:

CSV header looks like this:
Book Id,Title,Author,Author l-f,Additional Authors,ISBN,ISBN13,My Rating,Average Rating,Publisher,Binding,Number of Pages,Year Published,Original Publication Year,Date Read,Date Added,Bookshelves,Bookshelves with positions,Exclusive Shelf,My Review,Spoiler,Private Notes,Read Count,Recommended For,Recommended By,Owned Copies,Original Purchase Date,Original Purchase Location,Condition,Condition Description,BCID

Example CSV line:
8535464,"The Geeks Shall Inherit the Earth: Popularity, Quirk Theory and Why Outsiders Thrive After High School","Alexandra Robbins","Robbins, Alexandra","",="1401302025",="9781401302023",2,"3.62","Hyperion","Hardcover","448",2009,2009,2011/10/15,2012/07/16,"","","read","Found it only OK. Basically extended anecdotes, with some light science mixed in to buttress her manifesto (and used for support, not illumination).","","","","","",0,,,,,
-}
-- Background on Amazon: Generic ISBN search looks like this: http://www.amazon.com/gp/search/ref=sr_adv_b/?search-alias=stripbooks&unfiltered=1&field-isbn=9781401302023

{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<*>), (<$>))
import Data.ByteString.Lazy.Search as BLS (replace)
import Data.Csv (FromNamedRecord(..), (.:), decodeByName)
import Data.List as L (sortBy)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import System.Environment (getArgs)
import Text.Pandoc (Pandoc(..), Inline(..), Block(..), readMarkdown, writeMarkdown, strictExtensions, def, ReaderOptions(..))
import Text.Pandoc.Builder as TPB (fromList, simpleTable, singleton, toList, Blocks)
import qualified Data.ByteString.Lazy as B (readFile, ByteString)
import qualified Data.Map as M (fromList, lookup, Map)
import qualified Data.Vector as V (toList, Vector)

main :: IO ()
main = do books <- fmap head getArgs >>= B.readFile
          let books' = BLS.replace ",=\"" (",\""::B.ByteString) books
          case decodeByName books' of
              Left err -> putStrLn err
              Right (_, v) -> putStrLn (writeMarkdown def (Pandoc undefined [bookTable v]))

data GoodReads = GoodReads { title :: String,
                             author :: String,
                             isbn :: Maybe String, -- ISBNs aren't integers! eg '981068276X'
                             myRating :: Int,
                             yearPublished :: Maybe Int,
                             originalYearPublished :: Maybe Int,
                             dateRead :: String,
                             review :: String }
instance FromNamedRecord GoodReads where
  parseNamedRecord m = GoodReads <$>
                          m .: "Title" <*>
                          m .: "Author" <*>
                          m .: "ISBN" <*>
                          m .: "My Rating" <*>
                          m .: "Year Published" <*>
                          m .: "Original Publication Year" <*>
                          m .: "Date Read" <*>
                          m .: "My Review"

bookTable :: V.Vector GoodReads -> Block
bookTable books = let sorted = L.sortBy rating $ V.toList books
                      rows = map bookToRow sorted
                      in head (TPB.toList (simpleTable colHeaders rows))
         -- descending: 5 stars first; break ties with reviews
   where rating b1 b2
           | myRating b1 < myRating b2 = GT
           | myRating b1 == myRating b2 =
              if length (review b1) < length (review b2) then GT else LT
           | otherwise = LT

colHeaders :: [Blocks]
colHeaders = Prelude.map TPB.singleton [Plain [Str "Title"],
                                        Plain [Str "Author"],
                                        Plain [Str "Rating"],
                                        Plain [Str "Year"],
                                        Plain [Str "Read"],
                                        Plain [Str "Review"]]

bookToRow :: GoodReads -> [Blocks]
bookToRow gr = if  myRating gr == 0 then [] else -- 0 as rating means unread/unrated
                  Prelude.map TPB.singleton [Plain [Emph (titleOrIsbnToLink (title gr) (isbn gr))],
                                          Plain [Str (author gr)],
                                          Plain [Str (handleRating (myRating gr))],
                                          Plain [Str (handleDate gr)],
                                          Plain [Str (dateRead gr)]] ++
                  [TPB.fromList $ handleReview $ review gr]

handleRating :: Int -> String
handleRating stars = replicate stars '★'
handleDate :: GoodReads -> String
handleDate gr = show $ head $ catMaybes [yearPublished gr, originalYearPublished gr, Just 0]
handleReview :: String -> [Block]
handleReview rvw = let (Pandoc _ x) = readMarkdown def { readerExtensions = strictExtensions } rvw in x

-- Most books have an ISBN; if there is an ISBN, we want to link to the Amazon
-- search page so readers can find out more about it (and hopefully buy it). On
-- the other hand, there may well be no ISBN - it's online fanfiction, a visual
-- novel not released in America, a short story, a novella, etc. In that case,
-- we see if there's a manually-specified known URL for the title; and if there isn't anything in
-- the map, then we just don't link the title at all.
titleOrIsbnToLink :: String -> Maybe String -> [Inline]
titleOrIsbnToLink ttle i = let url = case i of
                                                 Just i' -> getAmazonPage i'
                                                 Nothing -> case M.lookup ttle isbnDB of
                                                                  Just i'' -> getAmazonPage i''
                                                                  Nothing -> case M.lookup ttle urlDB of
                                                                                  Just url'' -> url''
                                                                                  -- warn
                                                                                  Nothing -> trace ("Error! " ++ ttle) ""
                      in [Link [Str ttle] (url, "")]
                where getAmazonPage :: String -> String
                      getAmazonPage ibn = "http://www.amazon.com/s?ie=UTF8&field-isbn=" ++ ibn ++ "&page=1&rh=i:stripbooks"

isbnDB, urlDB :: M.Map String String
isbnDB = M.fromList [
     ("A Study of History, Vol 1: Introduction; The Geneses of Civilizations (A Study of History, #1)", "978-0195050806"),
     ("Economic Analysis of the Law: Selected Readings", "978-0631231585 "),
     ("Ender In Exile", "0765344157"),
     ("Ex-Prodigy: My Childhood and Youth", "0262730081"),
     ("Freakonomics", "0061234001"),
     ("How to Succeed in Evil", "0983097615"),
     ("Joel on Software", "1590593898"),
     ("Neon Genesis Evangelion, Vol. 02", "1421553058"),
     ("Nine Hundred Grandmothers ", "1880448971"),
     ("Prelude to Foundation (Foundation: Prequel, #1)", "9780586071113"),
     ("Reason & Persuasion: Three Dialogues By Plato", "981068276X"),
     ("Red Mars (Mars Trilogy, #1)", "0553560735"),
     ("The Many-Coloured Land (Saga of Pliocene Exile, #1)", "1433224097"),
     ("Why I Am Not a Christian: Four Conclusive Reasons to Reject the Faith ", "1456588850")
     ]
urlDB = M.fromList [
     ("A Study in Emerald", "http://www.neilgaiman.com/mediafiles/exclusive/shortstories/emerald.pdf"),
     ("Alliance of the Golden Witch (Umineko no Naku Koro ni #4)", "https://en.wikipedia.org/wiki/Umineko:_When_They_Cry"),
     ("Banquet of the Golden Witch (Umineko no Naku Koro ni #3)", "https://en.wikipedia.org/wiki/Umineko:_When_They_Cry"),
     ("Battlefield Earth", "https://en.wikipedia.org/wiki/Battlefield_Earth_(novel)"),
     ("Collected Poems of Robert Frost", "https://en.wikipedia.org/wiki/Collected_Poems_of_Robert_Frost_%281930%29"),
     ("Cybernetics", "https://en.wikipedia.org/wiki/Cybernetics,_or_Control_and_Communication_in_the_Animal_and_the_Machine"),
     ("Dawn of the Golden Witch (Umineko no Naku Koro ni Chiru #6)", "https://en.wikipedia.org/wiki/Umineko:_When_They_Cry"),
     ("End of the Golden Witch (Umineko no Naku Koro ni Chiru #5)", "https://en.wikipedia.org/wiki/Umineko:_When_They_Cry"),
     ("Ethics ", "https://en.wikipedia.org/wiki/Nicomachean_Ethics"),
     ("Exhalation", "http://www.nightshadebooks.com/Downloads/Exhalation%20-%20Ted%20Chiang.html"),
     ("Fallout: Equestria", "http://falloutequestria.wikia.com/wiki/Fallout:_Equestria"),
     ("Friendship is Optimal", "http://www.fimfiction.net/story/62074/friendship-is-optimal"),
     ("Harry Potter and the Methods of Rationality", "http://hpmor.com/"),
     ("Harry Potter and the Natural 20", "https://www.fanfiction.net/s/8096183/1/Harry-Potter-and-the-Natural-20"),
     ("Hell is the Absence of God", "http://www.ibooksonline.com/88/Text/hell.html"),
     ("If on a winter's night a traveler", "https://en.wikipedia.org/wiki/If_on_a_winter%27s_night_a_traveler"),
     ("It's behind you - The making of a computer Game", "http://bizzley.com/"),
     ("Legend of the Golden Witch (Umineko no Naku Koro ni #1)", "https://en.wikipedia.org/wiki/Umineko:_When_They_Cry"),
     ("Luminosity", "http://luminous.elcenia.com/"),
     ("Monstrous Regiment", "https://en.wikipedia.org/wiki/Monstrous_Regiment_%28novel%29"),
     ("Mother Earth Mother Board", "http://archive.wired.com/wired/archive/4.12/ffglass_pr.html"),
     ("Orphan Of The Helix", "http://bookre.org/reader?file=283363"),
     ("Radiance", "http://luminous.elcenia.com/radiance/ch1.shtml"),
     ("Requiem of the Golden Witch (Umineko no Naku Koro ni Chiru #7) ", "https://en.wikipedia.org/wiki/Umineko:_When_They_Cry"),
     ("Siddhartha", "https://en.wikipedia.org/wiki/Siddhartha_%28novel%29"),
     ("Sodom and Gomorrah, Texas", "http://www.gutenberg.org/ebooks/23161"),
     ("The Authoritarians", "http://members.shaw.ca/jeanaltemeyer/drbob/TheAuthoritarians.pdf"),
     ("The Constant Gardener", "https://en.wikipedia.org/wiki/The_Constant_Gardener"),
     ("The Fall of Rome", "http://www.amazon.com/The-fall-Rome-R-Lafferty/dp/B0006CALC4"),
     ("The Fall of the House of Usher", "https://en.wikipedia.org/wiki/The_Fall_of_the_House_of_Usher"),
     ("The Grand Inquisitor", "https://en.wikipedia.org/wiki/The_Grand_Inquisitor"),
     ("The Last Ringbearer", "https://en.wikipedia.org/wiki/The_Last_Ringbearer"),
     ("The Making of Prince of Persia", "http://jordanmechner.com/ebook/"),
     ("The Mysterious Island  ", "https://en.wikipedia.org/wiki/The_Mysterious_Island"),
     ("The Mysterious Stranger", "https://en.wikipedia.org/wiki/The_Mysterious_Stranger"),
     ("The Red Castle", "https://en.wikipedia.org/wiki/H._C._Bailey"),
     ("The Spy Who Came In from the Cold", "https://en.wikipedia.org/wiki/The_Spy_Who_Came_in_from_the_Cold"),
     ("The Strange Case of Dr. Jekyll and Mr. Hyde", "https://en.wikipedia.org/wiki/Strange_Case_of_Dr_Jekyll_and_Mr_Hyde"),
     ("The Stranger", "https://en.wikipedia.org/wiki/The_Stranger_%28novel%29"),
     ("The Sword of Good", "http://lesswrong.com/lw/169/the_sword_of_good/"),
     ("The Three Musketeers", "https://en.wikipedia.org/wiki/The_Three_Musketeers"),
     ("The Tommyknockers", "https://en.wikipedia.org/wiki/The_Tommyknockers"),
     ("Three Worlds Collide", "http://lesswrong.com/lw/y4/three_worlds_collide_08/"),
     ("Time Enough for Love (The World As Myth)", "https://en.wikipedia.org/wiki/Time_Enough_for_Love"),
     ("Town of Cats", "http://www.newyorker.com/fiction/features/2011/09/05/110905fi_fiction_murakami?currentPage=all"),
     ("Turn of the Golden Witch (Umineko no Naku Koro ni #2)", "https://en.wikipedia.org/wiki/Umineko:_When_They_Cry"),
     ("Twilight of the Golden Witch (Umineko no Naku Koro ni Chiru #8) ", "https://en.wikipedia.org/wiki/Umineko:_When_They_Cry"),
     ("Understand", "http://www.infinityplus.co.uk/stories/under.htm"),
     ("Utopia", "https://en.wikipedia.org/wiki/Utopia_%28book%29"),
     ("Waiting for Godot", "https://en.wikipedia.org/wiki/Waiting_for_Godot"),
     ("Wired Love: A Romance of Dots and Dashes", "https://encrypted.google.com/books?id=BjAOAAAAYAAJ"),
     ("The Story of Life Insurance", "http://en.wikisource.org/wiki/The_Story_of_Life_Insurance")
     ]
