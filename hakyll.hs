#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (arr, (>>>), (>>^))
import Data.FileStore (darcsFileStore)
import Data.List (elemIndex, isInfixOf, isPrefixOf)
import Data.Maybe (fromJust)
import Network.HTTP (urlEncode)
import Network.URI (unEscapeString, isUnescapedInURI, isURI)
import Network.URL (encString)
import System.Directory (copyFile)
import System.FilePath (hasExtension, takeExtension)
import System.Process (runCommand)
import qualified Data.Map as M (fromList, lookup, Map)

import Hakyll
import Network.Gitit.Feed (filestoreToXmlFeed, FeedConfig(..))
import Text.Pandoc (bottomUp, defaultWriterOptions, HTMLMathMethod(MathML), Inline(Link, Str), Pandoc, WriterOptions(..))
import Text.Pandoc.Shared (ObfuscationMethod(NoObfuscation))

-- TODO: is it a good thing to have my pages ending in .html? maybe I should
-- switch completely over to extensionless pages and use .htaccess redirects
main :: IO ()
main = do  hakyll $ do
             let static = route idRoute >> compile copyFileCompiler
             mapM_ (flip match static) ["docs/**",
                                       "images/**",
                                       "**.hs",
                                       "_darcs/**",
                                       "static/**"]

             match "**.css" $ route idRoute >> compile compressCssCompiler

             group "html" $ match "**.page" $ do
               route $ setExtension "html"
               compile $ myPageCompiler

                 >>> requireA "templates/sidebar.markdown" (setFieldA "sidebar" $ arr pageBody)
                 >>> applyTemplateCompiler "templates/default.html"
                 >>> relativizeUrlsCompiler

             match "templates/default.html" $ compile templateCompiler
             match "templates/sidebar.markdown" $ compile pageCompiler

           writeFile "_site/atom.xml" =<< filestoreToXmlFeed rssConfig (darcsFileStore "./")  Nothing

           -- Apache configuration (caching, compression)
           copyFile ".htaccess" "_site/.htaccess"

           _ <- runCommand "find _site/ -name \"*.css\"  -type f -exec /bin/sh -c \"gzip --stdout --best --rsyncable \\\"{}\\\" > \\\"{}.gz\\\"\" \\;"
           _ <- runCommand "find _site/ -name \"*.html\" -type f -exec /bin/sh -c \"gzip --stdout --best --rsyncable \\\"{}\\\" > \\\"{}.gz\\\"\" \\;"
           _ <- runCommand "find _site/ -name \"*.js\"   -type f -exec /bin/sh -c \"gzip --stdout --best --rsyncable \\\"{}\\\" > \\\"{}.gz\\\"\" \\;"
           return ()

options = defaultWriterOptions{ writerSectionDivs = True,
                                       writerStandalone = True,
                                       writerTableOfContents = True,
                                       writerTemplate = "$if(toc)$\n$toc$\n$endif$\n$body$",
                                       writerHTMLMathMethod = Text.Pandoc.MathML Nothing,
                                       writerEmailObfuscation = NoObfuscation }

rssConfig :: FeedConfig
rssConfig =  FeedConfig { fcTitle = "Joining Clouds", fcBaseUrl  = "http://www.gwern.net", fcFeedDays = 30 }

myPageCompiler = cached "myPageCompiler" $ readPageCompiler >>> addDefaultFields >>> arr applySelf >>> myPageRenderPandocWith
myPageRenderPandocWith = pageReadPandocWith defaultHakyllParserState >>^ fmap pandocTransform >>^ fmap (writePandocWith options)

pandocTransform :: Pandoc -> Pandoc
pandocTransform = bottomUp (map (convertInterwikiLinks))

-- | Derives a URL from a list of Pandoc Inline elements.
inlinesToURL :: [Inline] -> String
inlinesToURL = encString False isUnescapedInURI . inlinesToString

-- | Convert a list of inlines into a string.
inlinesToString :: [Inline] -> String
inlinesToString = concatMap go
  where go x = case x of
               Str s                   -> s
               _                       -> " "

convertInterwikiLinks :: Inline -> Inline
convertInterwikiLinks (Link ref (interwiki, article)) =
  case interwiki of
    ('!':interwiki') ->
        case M.lookup interwiki' interwikiMap of
                Just url  -> case article of
                                  "" -> Link ref (url `interwikiurl` (inlinesToString ref), summary $ unEscapeString $ inlinesToString ref)
                                  _  -> Link ref (url `interwikiurl` article, summary article)
                Nothing -> Link ref (interwiki, article)
            where -- 'http://starwars.wikia.com/wiki/Emperor_Palpatine'
                  -- TODO: `urlEncode` breaks Unicode strings like "Shōtetsu"!
                  interwikiurl u a = u ++ urlEncode a
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
wpInterwikiMap = [
                 ("Commons", "http://commons.wikimedia.org/wiki/"),
                 ("EmacsWiki", "http://www.emacswiki.org/cgi-bin/wiki.pl?"),
                 ("Google", "http://www.google.com/search?q="),
                 ("Wikimedia", "http://wikimediafoundation.org/wiki/"),
                 ("Wikinews", "http://en.wikinews.org/wiki/"),
                 ("Wikipedia", "http://en.wikipedia.org/wiki/"),
                 ("Wikiquote", "http://en.wikiquote.org/wiki/"),
                 ("Wikischool", "http://www.wikischool.de/wiki/"),
                 ("Wikisource", "http://en.wikisource.org/wiki/"),
                 ("Wiktionary", "http://en.wiktionary.org/wiki/"),
                 ("WMF", "http://wikimediafoundation.org/wiki/"),
                 ("Wookieepedia", "http://starwars.wikia.com/wiki/") ]
