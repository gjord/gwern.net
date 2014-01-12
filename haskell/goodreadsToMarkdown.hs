#! /usr/bin/env runhaskell
-- $ cabal install cassava stringsearch
-- $ wget http://www.gwern.net/docs/gwern-goodreads.csv
-- # Demonstration usage:
-- $ ./goodreadsToMarkdown.hs gwern-goodreads.csv > book-reviews.page

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
import System.Environment (getArgs)
import Text.Pandoc -- (Pandoc(..), Inline(..), Block(..), readMarkdown, writeMarkdown, strictExtensions, def, nullMeta, ReaderOptions(..))
import Text.Pandoc.Builder as TPB (fromList, simpleTable, singleton, toList, Blocks)
-- import Text.Pandoc.Definition (nullMeta)
import qualified Data.ByteString.Lazy as B (readFile, ByteString)
import qualified Data.Vector as V (toList, Vector)

main :: IO ()
main = do books <- fmap head getArgs >>= B.readFile
          let books' = BLS.replace ",=\"" (",\""::B.ByteString) books
          case decodeByName books' of
              Left err -> putStrLn err
              Right (_, v) -> putStrLn (writeMarkdown def (Pandoc undefined [bookTable v]))

data GoodReads = GoodReads { title :: String, author :: String, isbn :: Maybe Int,
                             myRating :: Int,
                             yearPublished :: Maybe Int, originalYearPublished :: Maybe Int,
                             dateRead :: String, review :: String }
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
                  Prelude.map TPB.singleton [Plain [Emph [Link [Str (title gr)] (handleISBN (isbn gr))]],
                                          Plain [Str (author gr)],
                                          Plain [Str (handleRating (myRating gr))],
                                          Plain [Str (handleDate gr)],
                                          Plain [Str (dateRead gr)]] ++
                  [TPB.fromList $ handleReview $ review gr]

handleISBN :: Maybe Int -> (String,String)
handleISBN i = case i of
                Nothing -> ("","")
                Just i' -> (getAmazonPage i', "ISBN: " ++ show i')

                where getAmazonPage :: Int -> String
                      getAmazonPage ibn = "http://www.amazon.com/gp/search/ref=sr_adv_b/?search-alias=stripbooks&unfiltered=1&field-isbn=" ++ show ibn

handleRating :: Int -> String
handleRating stars = replicate stars '★'
handleDate :: GoodReads -> String
handleDate gr = show $ head $ catMaybes [yearPublished gr, originalYearPublished gr, Just 0]
handleReview :: String -> [Block]
handleReview rvw = let (Pandoc _ x) = readMarkdown def { readerExtensions = strictExtensions } rvw in x
