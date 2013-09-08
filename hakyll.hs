#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Codec.Binary.UTF8.String (encode)
import Control.Arrow (arr, (>>>), (>>^))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (isAlphaNum, isAscii)
import Data.FileStore (darcsFileStore)
import Data.FileStore.Utils (runShellCommand)
import Data.List (isInfixOf, nub, sort)
import Data.Monoid (mempty, mconcat)
import Network.HTTP (urlEncode)
import Network.URI (unEscapeString)
import Text.HTML.TagSoup (renderTagsOptions,parseTags,renderOptions, optMinimize, Tag(TagOpen))
import Text.Printf (printf)
import qualified Data.Map as M (fromList, lookup, Map)
import System.IO.Unsafe (unsafePerformIO)

import Hakyll
import Feed (filestoreToXmlFeed, FeedConfig(..))
import Text.Pandoc (bottomUp, defaultWriterOptions, HTMLMathMethod(MathML), Inline(Code, Link, Str), Pandoc, WriterOptions(..))
import Text.Pandoc.Shared (ObfuscationMethod(NoObfuscation))

main :: IO ()
main = do  hakyll $ do
             -- handle the simple non-.page files
             let static = route idRoute >> compile copyFileCompiler
             mapM_ (`match` static) ["docs/**",
                                     "haskell/**",
                                     "images/**",
                                     "**.hs",
                                     "static/*",
                                     "static/img/**",
                                     "static/js/**"]
             _ <- match "**.css" $ route idRoute >> compile compressCssCompiler
             _ <- match "static/templates/*.html" $ compile templateCompiler

             -- handle the much more complex content pages, with tags & metadata etc.
             pages <- group "html" $ match "**.page" $ do
               route $ setExtension "" -- cool URLs
               compile $ myPageCompiler
                 >>> renderTagsField "prettytags" (fromCapture "tags/*" . escape)
                 >>> arr (trySetField "author" "gwern") -- only docs/*.page set 'author:'
                 >>> arr (trySetField "status" "N/A")
                 >>> arr (trySetField "belief" "N/A")
                 >>> renderModificationTime "modified" "%d %b %Y" -- populate $modified$
                 >>> applyTemplateCompiler "static/templates/default.html"

             -- Add a tag list compiler for every tag
             _ <- create "tags" $ requireAll pages (\_ ps -> readTags ps :: Tags String)
             match "tags/*" $ route $ setExtension ""
             metaCompile $ require_ "tags"
                 >>> arr tagsMap
                 >>> arr (map (\(t, p) -> (fromCapture "tags/*" t, makeTagList t p)))

           putStrLn "generating & copying RSS feed..."
           writeFile "_site/atom.xml" =<< filestoreToXmlFeed rssConfig (darcsFileStore "./")  Nothing

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "static/templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> applyTemplateCompiler "static/templates/tags.html"
        >>> relativizeUrlsCompiler

options :: WriterOptions
options = defaultWriterOptions{ writerSectionDivs = True,
                                writerStandalone = True,
                                writerTableOfContents = True,
                                writerColumns = 120,
                                writerTemplate = "<div id=\"TOC\">$toc$</div>\n$body$",
                                writerHtml5 = True,
                                writerHTMLMathMethod = Text.Pandoc.MathML Nothing,
                                writerEmailObfuscation = NoObfuscation }

rssConfig :: FeedConfig
rssConfig =  FeedConfig { fcTitle = "Joining Clouds", fcBaseUrl  = "http://www.gwern.net", fcFeedDays = 30 }

myPageCompiler :: Compiler Resource (Page String)
myPageCompiler = cached "myPageCompiler" (readPageCompiler >>> addDefaultFields >>> arr (changeField "description" escapeHtml) >>> arr applySelf >>> myPageRenderPandocWith)

myPageRenderPandocWith :: Compiler (Page String) (Page String)
myPageRenderPandocWith = pageReadPandocWith defaultHakyllParserState >>^ fmap pandocTransform >>^ fmap (writePandocWith options) >>^ fmap (unsafePerformIO . addImgDimensions)

pandocTransform :: Pandoc -> Pandoc
pandocTransform = bottomUp (map (convertInterwikiLinks . convertHakyllLinks . addAmazonAffiliate))

addAmazonAffiliate :: Inline -> Inline
addAmazonAffiliate x@(Link r (l, t)) =  if "amazon.com/" `isInfixOf` l && not ("?tag=" `isInfixOf` l)
                                        then Link r (l++"?tag=gwernnet-20", t) else x
addAmazonAffiliate x = x

-- GITIT -> HAKYLL LINKS PLUGIN
-- | Convert links with no URL to wikilinks.
convertHakyllLinks :: Inline -> Inline
convertHakyllLinks (Link ref ("", "")) = let ref' = inlinesToURL ref in Link ref (ref', "Go to wiki page: " ++ ref')
convertHakyllLinks x = x

-- FASTER HTML RENDERING BY STATICLY SPECIFYING ALL IMAGE DIMENSIONS
-- read HTML string with TagSoup, process `<img>` tags to read the file's dimensions, and hardwire them
-- this optimizes HTML rendering since browsers know before downloading the image how to layout the page
addImgDimensions :: String -> IO String
addImgDimensions = fmap (renderTagsOptions renderOptions{optMinimize=whitelist}) . mapM staticImg . parseTags
                 where whitelist s = s /= "div" && s /= "script"
{- example illustration:
 TagOpen "img" [("src","/images/201201-201207-traffic-history.png")
                ("alt","Plot of page-hits (y-axis) versus date (x-axis)")],
 TagOpen "figcaption" [],TagText "Plot of page-hits (y-axis) versus date (x-axis)",
 TagClose "figcaption",TagText "\n",TagClose "figure" -}
staticImg :: Tag String -> IO (Tag String)
staticImg x@(TagOpen "img" xs) = do let path = lookup "src" xs
                                    case path of
                                           Nothing -> return x
                                           Just p -> do let p' = if head p == '/' then tail p else p
                                                        print p'
                                                        (height,width) <- imageMagick p'
                                                        return (TagOpen "img" (uniq ([("height",height), ("width",width)]++xs)))
            where uniq = nub . sort
staticImg x = return x
-- | Use FileStore util to run imageMagick's 'identify', & extract the dimensions
imageMagick :: FilePath -> IO (String,String)
imageMagick f = do (_,_,bs) <- runShellCommand "./" Nothing "identify" ["-format", "%h %w", f]
                   let [height,width] = words (unpack bs)
                   return (height,width)


-- INTERWIKI PLUGIN
-- | Derives a URL from a list of Pandoc Inline elements.
inlinesToURL :: [Inline] -> String
inlinesToURL x = let x' = inlinesToString x
                     (a,b) = break (=='%') x'
                 in escape a ++ b
-- copied from "XMonad.Actions.Search"
escape :: String -> String
escape = concatMap escapeURIChar
         where escapeURIChar :: Char -> String
               escapeURIChar c | isAscii c && isAlphaNum c = [c]
                               | otherwise                 = concatMap (printf "%%%02X") $ encode [c]

-- | Convert a list of inlines into a string.
inlinesToString :: [Inline] -> String
inlinesToString = concatMap go
  where go x = case x of
               Str s    -> s
               Code _ s -> s
               _        -> " "
convertInterwikiLinks :: Inline -> Inline
convertInterwikiLinks (Link ref (interwiki, article)) =
  case interwiki of
    ('!':interwiki') ->
        case M.lookup interwiki' interwikiMap of
                Just url  -> case article of
                                  "" -> Link ref (url `interwikiurl` inlinesToString ref, summary $ unEscapeString $ inlinesToString ref)
                                  _  -> Link ref (url `interwikiurl` article, summary article)
                Nothing -> Link ref (interwiki, article)
            where -- 'http://starwars.wikia.com/wiki/Emperor_Palpatine'
                  interwikiurl u a = u ++ urlEncode (deunicode a)
                  deunicode = map (\x -> if x == '’' then '\'' else x)
                  -- 'Wookieepedia: Emperor Palpatine'
                  summary a = interwiki' ++ ": " ++ a
    _ -> Link ref (interwiki, article)
convertInterwikiLinks x = x
-- | Large table of constants; this is a mapping from shortcuts to a URL. The URL can be used by
--   appending to it the article name (suitably URL-escaped, of course).
interwikiMap :: M.Map String String
interwikiMap = M.fromList $ wpInterwikiMap ++ customInterwikiMap
wpInterwikiMap, customInterwikiMap :: [(String, String)]
customInterwikiMap = [("Hackage", "http://hackage.haskell.org/package/"),
                      ("Hawiki", "http://haskell.org/haskellwiki/"),
                      ("Hayoo", "http://holumbus.fh-wedel.de/hayoo/hayoo.html#0:"),
                      ("Hoogle", "http://www.haskell.org/hoogle/?hoogle=")]
wpInterwikiMap = [ ("Commons", "http://commons.wikimedia.org/wiki/"),
                 ("EmacsWiki", "http://www.emacswiki.org/cgi-bin/wiki.pl?"),
                 ("Wikimedia", "http://wikimediafoundation.org/wiki/"),
                 ("Wikipedia", "http://en.wikipedia.org/wiki/"),
                 ("Wikiquote", "http://en.wikiquote.org/wiki/"),
                 ("Wikisource", "http://en.wikisource.org/wiki/"),
                 ("Wiktionary", "http://en.wiktionary.org/wiki/"),
                 ("WMF", "http://wikimediafoundation.org/wiki/"),
                 ("Wookieepedia", "http://starwars.wikia.com/wiki/") ]
