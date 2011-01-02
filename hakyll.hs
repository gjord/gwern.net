import Control.Arrow (arr, (>>>), (&&&))
import Control.Monad (liftM)
import Data.List (elemIndex, isInfixOf, isPrefixOf, sort)
import Data.Maybe (fromJust)
import Network.HTTP (urlEncode)
import Network.URI (isURI, unEscapeString, isUnescapedInURI)
import Network.URL (encString)
import System.FilePath (hasExtension, takeExtension)
import System.Directory (removeFile)
import qualified Data.Map as M (fromList, lookup, Map)

import Text.Hakyll
import Text.Pandoc (bottomUp, defaultParserState, defaultWriterOptions, readMarkdown, writeHtmlString,
                    HTMLMathMethod(MathML), Inline(Link, Str), Pandoc, WriterOptions(..))
import Text.Pandoc.Shared (ObfuscationMethod(NoObfuscation))

import Network.Gitit.Feed (filestoreToXmlFeed, FeedConfig(..))
import Data.FileStore (darcsFileStore)

main :: IO ()
main = do
    -- set up RSS
    atom <- filestoreToXmlFeed rssConfig (darcsFileStore "./")  Nothing
    let feed = "atom.xml"
    writeFile feed atom

    hakyll "http://gwern.net" $ do

        _ <- forkHakyllWait $ directory css "css"
        mapM_ (directory static) ["_darcs",
                                  "/home/gwern/_darcs",
                                  "/home/gwern/bin/hcorpus",
                                  "/home/gwern/bin/archiver",
                                  "images",
                                  "docs",
                                  "static"]
        static feed -- cp RSS in

        pages <- liftM sort $ getRecursiveContents "./"
    
        let articles = havingExtension ".page" pages
        let sources = havingExtension ".hs" pages

        -- TODO: make this faster - 'forkHakyll'?
        mapM_ (render' ["templates/default.html"]) (articles++sources)

    -- back in the original RSS 'do'; we clean up after ourselves
    removeFile feed 

rssConfig :: FeedConfig
rssConfig =  FeedConfig {
                        fcTitle = "Joining Clouds"
                        , fcBaseUrl  = "http://www.gwern.net"
                        , fcFeedDays = 10
                        }

render' :: [FilePath] -> FilePath -> Hakyll ()
render' templates = renderChain templates  . withSidebar . page
 where
     withSidebar :: HakyllAction () Context -> HakyllAction () Context
     withSidebar a = a `combine` createPage "templates/sidebar.markdown"

page :: FilePath -> HakyllAction () Context
page pg = readPageAction pg >>> (arr id &&& arr (const total)) >>> renderActionWith
   where
       total :: String -> String
       total = writeHtmlString options . changeLinks . readMarkdown defaultParserState

       options = defaultWriterOptions{ writerStandalone = True,
                                     writerTableOfContents = True,
                                     writerTemplate = "$if(toc)$\n$toc$\n$endif$\n$body$",
                                     writerHTMLMathMethod = Text.Pandoc.MathML Nothing,
                                     writerEmailObfuscation = NoObfuscation }

       changeLinks :: Pandoc -> Pandoc
       changeLinks = bottomUp (map (convertEmptyWikiLinks . convertInterwikiLinks))

-- | Convert links with no URL to wikilinks.
convertEmptyWikiLinks :: Inline -> Inline
convertEmptyWikiLinks (Link ref ("", "")) =   let ref' = inlinesToURL ref in Link ref (transform ref', "Go to wiki page: " ++ ref')
convertEmptyWikiLinks (Link ref (y, x)) =  Link ref (transform y, x)
convertEmptyWikiLinks x = x

{- specification for 'transform':
test :: Bool
test = all (\(a,b) -> transform a == b) [
        ("!Hoogle 'foo'", "!Hoogle 'foo'"),
        ("!Wikipedia 'Multivitamin#Evidence against'", "!Wikipedia 'Multivitamin#Evidence against'"),
        ("!Wikipedia 'foo'", "!Wikipedia 'foo'"),
        ("#Benefits", "#Benefits"),
        ("Chernoff Faces", "Chernoff Faces.html"),
        ("In Defense of Inclusionism.html", "In Defense of Inclusionism.html"),
        ("N-back FAQ#hardcore", "N-back FAQ.html#hardcore"),
        ("Redirect-bot.hs", "Redirect-bot.hs"),
        ("Terrorism is not about Terror#the-problem", "Terrorism is not about Terror.html#the-problem"),
        ("doc/foo.pdf", "doc/foo.pdf"),
        ("docs/gwern.xml", "docs/gwern.xml"),
        ("docs/gwern.xml.gz", "docs/gwern.xml.gz"),
        ("http://en.wikipedia.org/wiki/Angst", "http://en.wikipedia.org/wiki/Angst"),
        ("http://en.wikipedia.org/wiki/Melatonin#Use%20as%20a%20dietary%20supplement", "http://en.wikipedia.org/wiki/Melatonin#Use%20as%20a%20dietary%20supplement"),
        ("http://en.wikipedia.org/wiki/Multivitamin#Evidence%20against", "http://en.wikipedia.org/wiki/Multivitamin#Evidence%20against"),
        ("http://www.google.com", "http://www.google.com"),
        ("http://www.gwern.net/N-back FAQ.html#fn1", "http://www.gwern.net/N-back FAQ.html#fn1"),
        ("sicp/Chapter 1.1", "sicp/Chapter 1.1.html"),
        ("sicp/Introduction", "sicp/Introduction.html")
        ]
-}
transform :: String -> String
transform y = let extension = drop 1 $ takeExtension y in
               if (length extension > 0) &&hasExtension y
               then if extension `notElem` map show [(0 :: Int) .. 9]
                    then y
                    else  y ++ ".html"
               else if ("!" `isPrefixOf` y) || ("#" `isPrefixOf` y) || isURI y
                      then y
                      else
                       if "#" `isInfixOf` y
                       then let (lnk, sctn) = splitAt (fromJust $ elemIndex '#' y) y in
                                lnk ++ ".html" ++ sctn
                       else y ++ ".html"

-- | Derives a URL from a list of Pandoc Inline elements.
inlinesToURL :: [Inline] -> String
inlinesToURL = encString False isUnescapedInURI . inlinesToString

-- | Convert a list of inlines into a string.
inlinesToString :: [Inline] -> String
inlinesToString = concatMap go
  where go x = case x of
               Str s                   -> s
               _                       -> " "
               -- apparently I can get away with this little for interwikis

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
-- full mapping available in Gitit interwiki plugin
wpInterwikiMap = [
                 ("Commons", "http://commons.wikimedia.org/wiki/"),
                 ("EmacsWiki", "http://www.emacswiki.org/cgi-bin/wiki.pl?"),
                 ("Google", "http://www.google.com/search?q="),
                 ("GoogleDefine", "http://www.google.com/search?q=define:"),
                 ("GoogleGroups", "http://groups.google.com/groups?q="),
                 ("gutenberg", "http://www.gutenberg.org/etext/"),
                 ("gutenbergwiki", "http://www.gutenberg.org/wiki/"),
                 ("JSTOR", "http://www.jstor.org/journals/"),
                 ("MemoryAlpha", "http://memory-alpha.org/en/wiki/"),
                 ("MetaWiki", "http://sunir.org/apps/meta.pl?"),
                 ("MetaWikiPedia", "http://meta.wikimedia.org/wiki/"),
                 ("MoinMoin", "http://moinmo.in/"),
                 ("OEIS", "http://www.research.att.com/~njas/sequences/"),
                 ("OSI", "http://wiki.tigma.ee/index.php/"),
                 ("OTRS", "https://ticket.wikimedia.org/otrs/index.pl?Action=AgentTicketZoom&TicketID="),
                 ("OTRSwiki", "http://otrs-wiki.wikimedia.org/wiki/"),
                 ("Scholar", "http://scholar.google.com/scholar?q="),
                 ("TVtropes", "http://www.tvtropes.org/pmwiki/pmwiki.php/Main/"),
                 ("Uncyclopedia", "http://uncyclopedia.org/wiki/"),
                 ("UseMod", "http://www.usemod.com/cgi-bin/wiki.pl?"),
                 ("wg", "http://wg.en.wikipedia.org/wiki/"),
                 ("Wiki", "http://c2.com/cgi/wiki?"),
                 ("Wikia", "http://www.wikia.com/wiki/c:"),
                 ("WikiaSite", "http://www.wikia.com/wiki/c:"),
                 ("Wikianso", "http://www.ansorena.de/mediawiki/wiki/"),
                 ("Wikible", "http://wikible.org/en/"),
                 ("Wikibooks", "http://en.wikibooks.org/wiki/"),
                 ("Wikichat", "http://www.wikichat.org/"),
                 ("WikiChristian", "http://www.wikichristian.org/index.php?title="),
                 ("Wikicities", "http://www.wikia.com/wiki/"),
                 ("Wikicity", "http://www.wikia.com/wiki/c:"),
                 ("WikiF1", "http://www.wikif1.org/"),
                 ("WikiFur", "http://en.wikifur.com/wiki/"),
                 ("wikiHow", "http://www.wikihow.com/"),
                 ("WikiIndex", "http://wikiindex.com/"),
                 ("WikiLemon", "http://wiki.illemonati.com/"),
                 ("Wikilivres", "http://wikilivres.info/wiki/"),
                 ("WikiMac-de", "http://apfelwiki.de/wiki/Main/"),
                 ("WikiMac-fr", "http://www.wikimac.org/index.php/"),
                 ("Wikimedia", "http://wikimediafoundation.org/wiki/"),
                 ("Wikinews", "http://en.wikinews.org/wiki/"),
                 ("Wikinfo", "http://www.wikinfo.org/index.php/"),
                 ("Wikinurse", "http://wikinurse.org/media/index.php?title="),
                 ("Wikinvest", "http://www.wikinvest.com/"),
                 ("Wikipaltz", "http://www.wikipaltz.com/wiki/"),
                 ("Wikipedia", "http://en.wikipedia.org/wiki/"),
                 ("WikipediaWikipedia", "http://en.wikipedia.org/wiki/Wikipedia:"),
                 ("Wikiquote", "http://en.wikiquote.org/wiki/"),
                 ("Wikireason", "http://wikireason.net/wiki/"),
                 ("Wikischool", "http://www.wikischool.de/wiki/"),
                 ("wikisophia", "http://wikisophia.org/index.php?title="),
                 ("Wikisource", "http://en.wikisource.org/wiki/"),
                 ("Wikispecies", "http://species.wikimedia.org/wiki/"),
                 ("Wikispot", "http://wikispot.org/?action=gotowikipage&v="),
                 ("Wikitech", "https://wikitech.wikimedia.org/view/"),
                 ("WikiTI", "http://wikiti.denglend.net/index.php?title="),
                 ("WikiTravel", "http://wikitravel.org/en/"),
                 ("WikiTree", "http://wikitree.org/index.php?title="),
                 ("Wikiversity", "http://en.wikiversity.org/wiki/"),
                 ("WikiWikiWeb", "http://c2.com/cgi/wiki?"),
                 ("Wiktionary", "http://en.wiktionary.org/wiki/"),
                 ("WMF", "http://wikimediafoundation.org/wiki/"),
                 ("Wookieepedia", "http://starwars.wikia.com/wiki/") ]
