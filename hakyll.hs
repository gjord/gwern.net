#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Codec.Binary.UTF8.String (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (isAlphaNum, isAscii)
import Data.List (isInfixOf, nub, sort)
import Data.Monoid ((<>))
import Data.Set (filter)
import Network.HTTP (urlEncode)
import Network.URI (unEscapeString)
import System.Directory (createDirectoryIfMissing)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import qualified Data.Map as M (fromList, lookup, Map)

import Data.FileStore (gitFileStore)
import Data.FileStore.Utils (runShellCommand)
import Feed (filestoreToXmlFeed, FeedConfig(..))
import Hakyll ((.&&.), applyTemplateList, buildTags, compile, complement, compressCssCompiler, constField,
               copyFileCompiler, dateField, defaultContext, defaultHakyllReaderOptions,
               defaultHakyllWriterOptions, fromCapture, getRoute, hakyll, idRoute, itemIdentifier,
               loadAll, loadAndApplyTemplate, loadBody, makeItem, match, modificationTimeField,
               pandocCompilerWithTransform, preprocess, relativizeUrls, route, setExtension,
               tagsField, tagsRules, templateCompiler, Compiler, Context, Item, Pattern, Tags)
import Text.HTML.TagSoup (renderTagsOptions,parseTags,renderOptions, optMinimize, Tag(TagOpen))
import Text.Pandoc (bottomUp, Extension(Ext_markdown_in_html_blocks), HTMLMathMethod(MathML), Inline(..),
                    ObfuscationMethod(NoObfuscation), Pandoc(..), ReaderOptions(..), WriterOptions(..))

main :: IO ()
main = hakyll $ do
             preprocess $ do rss <- filestoreToXmlFeed rssConfig (gitFileStore "./")  Nothing
                             createDirectoryIfMissing False "_site"
                             writeFile "_site/atom.xml" rss

             -- handle the simple static non-.page files
             let static = route idRoute >> compile copyFileCompiler
             mapM_ (`match` static) [ -- WARNING: match everything *except* Markdown
                                      -- since rules are mutually-exclusive!
                                     complement "docs/**.page" .&&. "docs/**",
                                     "haskell/**.hs",
                                     "images/**",
                                     "**.hs",
                                     "static/*",
                                     "static/img/**",
                                     "static/js/**"]
             match "**.css" $ route idRoute >> compile compressCssCompiler
             match "static/templates/*.html" $ compile templateCompiler

             tags <- buildTags "**.page" (fromCapture "tags/*")

             match "**.page" $ do
                 route $ setExtension "" -- cool URLs
                 -- https://groups.google.com/forum/#!topic/pandoc-discuss/HVHY7-IOLSs
                 let readerOptions = defaultHakyllReaderOptions { readerExtensions = Data.Set.filter (/=Ext_markdown_in_html_blocks) $ readerExtensions defaultHakyllReaderOptions }
                 compile $ pandocCompilerWithTransform readerOptions woptions pandocTransform
                     >>= loadAndApplyTemplate "static/templates/default.html" (postCtx tags)
                     >>= imgUrls
                     >>= relativizeUrls

             tagsRules tags $ \tag pattern -> do
                 let title = "Tag: " ++ tag
                 route idRoute
                 compile $ tagPage tags title pattern

woptions :: WriterOptions
woptions = defaultHakyllWriterOptions{ writerSectionDivs = True,
                                       writerStandalone = True,
                                       writerTableOfContents = True,
                                       writerColumns = 120,
                                       writerTemplate = "<div id=\"TOC\">$toc$</div>\n$body$",
                                       writerHtml5 = True,
                                       writerHTMLMathMethod = Text.Pandoc.MathML Nothing,
                                       writerEmailObfuscation = NoObfuscation }


rssConfig :: FeedConfig
rssConfig = FeedConfig { fcTitle = "Gwern", fcBaseUrl  = "http://www.gwern.net", fcFeedDays = 30 }

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags pattern preprocess' = do
    postItemTemplate <- loadBody "static/templates/postitem.html"
    posts' <- loadAll pattern
    posts <- preprocess' posts'
    applyTemplateList postItemTemplate (postCtx tags) posts
tagPage :: Tags -> String -> Pattern -> Compiler (Item String)
tagPage tags title pattern = do
    list <- postList tags pattern (return . id)
    makeItem ""
        >>= loadAndApplyTemplate "static/templates/tags.html"
                (constField "posts" list <> constField "title" title <>
                    defaultContext)
        >>= relativizeUrls

imgUrls :: Item String -> Compiler (Item String)
imgUrls item = do
    rte <- getRoute $ itemIdentifier item
    return $ case rte of
        Nothing -> item
        Just _  -> fmap (unsafePerformIO . addImgDimensions) item

postCtx :: Tags -> Context String
postCtx tags =
    tagsField "tags" tags <>
    defaultContext <>
    dateField "created" "%d %b %Y" <>
    modificationTimeField "modified" "%d %b %Y" <>
    constField "author" "gwern" <>
    constField "status" "N/A" <>
    constField "belief" "N/A" <>
    constField "description" "N/A"

pandocTransform :: Pandoc -> Pandoc
pandocTransform = bottomUp (map (convertInterwikiLinks . convertHakyllLinks . addAmazonAffiliate))

addAmazonAffiliate :: Inline -> Inline
addAmazonAffiliate (Link r (l, t)) | "?search" `isInfixOf` l                                 = Link r (l++"&tag=gwernnet-20", t)
                                   | "amazon.com/" `isInfixOf` l && not ("?tag=" `isInfixOf` l) = Link r (l++"?tag=gwernnet-20", t)
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
staticImg x@(TagOpen "img" xs) = do let optimized = lookup "height" xs
                                    case optimized of
                                      Just _ -> return x
                                      Nothing -> do let path = lookup "src" xs
                                                    case path of
                                                          Nothing -> return x
                                                          Just p -> do let p' = if head p == '/' then tail p else p
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
                      ("Hoogle", "http://www.haskell.org/hoogle/?hoogle=")]
wpInterwikiMap = [("Wikipedia", "http://en.wikipedia.org/wiki/"),
                  ("Wikiquote", "http://en.wikiquote.org/wiki/"),
                  ("Wiktionary", "http://en.wiktionary.org/wiki/")]
