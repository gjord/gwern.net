One of the best ways to learn something practical like Haskell is to simply dive in and make it do something useful for you. In my case, I've wanted to learn Haskell for a long time: the syntax was gorgeous compared to Common Lisp, sh, or Java (the only other languages I had any sort of experience with); various ideas such as monads, lazy evaluation, and purity struck me as fascinating and exciting; and the community was excellent in so far as I was smart enough to understand it.

But I didn't have any practical use for it, and I'm too lazy to learn it unless I have to. Bash worked well enough for my shell scripts, my Java days were thankfully far behind me, and I generally only messed with Common Lisp because of my use of the [Stump window manager](http://en.wikipedia/wiki/StumpWM), but I had finished configuring and extending StumpWM to my liking (and I use Xmonad these days). Fortunately, I am a heavy user and long-time editor of Wikipedia, and there have long been a number of technical things bugging me.

Two in particular struck me as fixable without requiring modifications to the MediaWiki codebase (a messy and featureful pile of PHP):

1) the "feature" of MediaWiki that by default, [[Fujiwara no Teika]] is distinct from [[Fujiwara no teika]], and going to the latter does not automatically redirect to the former. The reason this is very annoying is that one must either create n^2-1 redirects to the correct title or simply accept that mis-capitalizations will result in broken links and needless searches.
2) Links to other non-Wikipedia websites, "external links", often break. Because articles can evolve so fast, the Internet Archive doesn't necessarily manage to archive all external links; worse, the Internet Archive is dependent for its data on data dumps from Alexa, which are provided an eternity (6 months or more) after the original spidering of the external link. Thus, an external link could be added to an article, used as a reference, fall victim to the infamous 404 error, be noticed to be a dead link and removed on that basis within a few weeks much less 9 months!

Let's talk about Problem 2.

The beginning of a solution is to realize that the Internet Archive is not useful here. Their FAQ specifically says that archiving is done only through Alexa, and Alexa only does it through their spiders, the Alexa toolbar (proprietary and MS Windows/IE-only), and some web form which doesn't appear to do anything. Fortunately, if we poke around on the subject of archiving URLs, we'll eventually discover something like [WebCite](http://www.webcitation.org/), which is tailor-made for our purposes. They offer persistent archiving services, for free, and best of all they can archive on demand!

Reading through their documentation, it seems they will eventually allow you to upload a file containing a large number of URLs to cite, but not yet. That's alright - just trying out the archiving (or reading the technical FAQ) reveals the URL encodes everything in a very simple format; it's really just as simple as

> "http://www.webcitation.org/archive?url=" ++ url ++ "&email=foo@bar.com"

So going back to our problem. Our problem is that we want to take all the articles of the English Wikipedia, somehow extract all the external links from said articles, transform those  URLs according to the previous paragraph, and then actually open them (which will cause the embedded URL to be archived).

Our first question is naturally "How on Earth do we even get the addresses of all 1.7 million Wikipedia articles? Saying they're all somewhere in en.wikipedia.org/wiki/ doesn't help." I'm going to cop out a bit here and not actually explain how I hit upon this approach, but we'll borrow some code from the bot I coded to fix Problem 1, which looks a little like this:

> main = do
>     cd "/home/gwern/bin/pywikipedia/"
>     {- The list comprehension is to prevent from operating on page titles which will
>     cause enormous numbers of redirects to be created. Its what comes before that matters. -}
>     list <- liftM (\bs -> [n | n<-bs,  genericLength n <= (2^4)]) $ liftM words $ getContents

[Redirect-bot.hs]() is assuming here that stdin is the source of a list. This potentially extremely long (as in >9,555,933 lines) list takes this form:

 ...
 Delehaye
 Delekovac
 Delele
 Delemont
 Delemont_(Jura)
 Delemont_JU
 Delena
 Delena_cancerides
 Delenda
 Deleni
 ...

Notice that spaces are being escaped as underscores, and that each page name is on a separate line. We are getting all this information from a special file kindly provided by Wikipedia precisely for people like us, who don't want to spend the effort to parse the Special:Allpages <http://en.wikipedia/wiki/Special:Specialpages> to get the live list of article names; parsing Allpages is how all the serious Wikipedia bots work, but relying on old database dumps gets us 90% of the functionality at 10% the effort. Go visit <http://download.wikimedia.org/enwiki/latest/> and look for a file called 'all_titles_in_ns0.gz'. When it's downloaded and uncompressed to a ~60M file, it provides suitable fodder.

So, we have getContents lazily slurping in names of articles to standard in, and we have 'words' (it's a pure non-IO function, but liftM lets us use it in the IO monad) breaking  a single long string with lots of newlines into a list of strings.

> import Monad (liftM) -- Don't forget to import stuff!
> main :: IO ()
> main = do
>          articlelist <- liftM words $ getContents

Now what? Well, a list of items each of which we want to perform the exact same operation on almost cries out for us to use a (monadic) map on it.

>          urllist <- mapM fetchArticleURLs articlelist

But what *is* this mystery function 'fetchArticleText'? Well, obviously it will take as its single argument a string, and equally obviously it will return a list of strings since any given article might have 0, 1, or many different external links in it. And since we certainly aren't loading into memory an immutable copy of Wikipedia (regardless of whether we download a full dump of the articles or decide to fetch articles over the Internet), the result will be promoted into the IO monad. So the type signature must be:

> fetchArticleURLs :: String -> IO [String]

Purely arbitrarily, we'll have fetchArticleURLs work on pages from the live English Wikipedia; but you could probably also go the route of working from an offline database dump, which would have the advantage of halving network requests and parsing out URLs much faster (since there'd be no need to request individual pages over the network).

It's not too hard to see how to get the URL for any given article ("http://en.wikipedia.org/wiki/" ++ article), nor how to use Network.HTTP to download the HTML file constituting an article (couldn't be more than 10 lines) - but reliably parsing that HTML to extract genuine external links doesn't seem trivial at all! We could go with some sort of clumsy regexp matching on "http://", but hit-or-miss regular expressions just aren't The Haskell Way.

Stumped, the thing to do is to go bum around on the Haskell wiki and #haskell, where some excellent fellow will eventually inform us that "Hey, you know, parsing Wikipedia pages' HTML to extract out all external links kind of reminds me of one of the examples for Neil Mitchell's TagSoup library. You should go check it out."

Indeed, we need some sort of library to parse HTML for us and maybe handle the network business, and [TagSoup](http://www-users.cs.york.ac.uk/~ndm/tagsoup/) fits the bill admirably. One of the examples in Example.hs (googleTechNews) does almost precisely what we want, except for Google Tech News. The example in its entirety:

> googleTechNews = do
>        tags <- liftM parseTags $ openURL "http://news.google.com/?ned=us&topic=t"
>        let links = mapMaybe extract $ sections match tags
>        putStr $ unlines links
>    where extract xs = maybeTagText (xs !! 2)
>          match =
>              Match.tagOpenAttrNameLit "a" "id"
>               (\value -> "r" `isPrefixOf` value && 'i' `notElem` value)

Reading this, one is struck by 'openURL'; we don't even need to read the documentation to know what it does. We already have the beginning of our fetchArticleURLs definition:

> import Text.HTML.Download (openURL)
> fetchArticleURLs :: String -> IO [String]
> fetchArticleURLs article = ...something... $ openURL("http://en.wikipedia.org/wiki/" ++ article)

We could insert into the ellipsis whatever parsing we cook up, but we might want to use it somewhere else, and besides, it's bad practice to muddy up a function that could be pure (like our hypothetical extractURLs function) with IO. Separation of concerns, yay! So we're up to this:

> fetchArticleURLs article = liftM extractURLs $ openURL("http://en.wikipedia.org/wiki/" ++ article)
> extractURLs :: String -> [String]
> extractURLs arg = undefined

I'm afraid the definition of extractURLs is a bit magical; I don't entirely understand it. It's clear enough to me that one wants <href>s, which are listed under the 'TagOpen "a"' output, hence the first condition of the list comprehension, and the actual URL comes last, hence the second clause with the pattern matching, and the third filter is to make sure relative URLs are filtered out (when one makes an internal link to another Wikipedia article, it's not given fully qualified with the "http://en.wikipedia.org/" leader but starting with "/wiki/foo_bar"; just another example of a pervasive Unixism). But I simply couldn't've put it all together. Just another example of the power of #haskell:

> extractURLs arg = [x | TagOpen "a" atts <- (parseTags arg), (_,x) <- atts, "http://" `isPrefixOf` x]

Don't forget that all the new things need to be imported:

> import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
> import Data.List (isPrefixOf)

Oy. So we wrote extractURLs, which was used in fetchArticleURLs, which was used in main. Let's backtrack a bit; we had left main at:

> main = do
>          articlelist <- liftM words $ getContents
>          urllist <- mapM fetchArticleURLs articlelist

By the time urllist is generated, we've got an IO [String] which is containing all the external links. Now we can once again indulge the desire to map some sort of function over it. Another map, another hypothetical function:

>          mapM archiveURL urllist

For convenience's sake, we'll reuse openURL even though we really want something like this which will throw away the superfluous output:

> archiveURL :: String -> IO () -- Ideal

The simplest way to define it is this:

> archiveURL :: String -> IO String -- Reality
> archiveURL url = openURL("http://www.webcitation.org/archive?url=" ++ url ++ "&email=foo@bar.com")

But we can improve on this! We are using mapM which will go to all the trouble of retaining all the output from archiveURL being run on the dozens, hundreds, thousands, or millions of entries in urllist. Can't we be more efficient somehow? This sort of situation is exactly where naming conventions can come in handy. I happen to remember that just as foo' denotes some stricter version of a function foo, a foo_ denotes some sort of IO function that will throw away any output (learned that using zipWith_ in Redirect-bot.hs). One tries mapM_ instead, and it works!

We compile the entire program, and it is short. It is sweet. It works, and does what we want, and has explicit types and import statements, and we've reasoned out every bit of it. So we should be pretty happy with it. But I've noticed one thing while playing around in GHCi: it's naive and inefficient. Not only does it eat up ungodly amounts of memory and time, it also seems to be making duplicate requests! The reason for this is that on every Wikipedia webpage containing an article, there are a number of external links which aren't part of the article itself; this includes linking to the WikiMedia Foundation (the non-profit organization which runs all the Wikipedias and associated projects), links to donations, etc.

Well, not a hard problem. We want to ensure there are no duplicate entries in our list; one suspects this is a common desire in list processing, and yes, if we check Data.List we will find a function 'nub' which does exactly this. It makes the most sense to put nub just before we begin mapping archiveURL over urllist, so in it goes:

>          mapM_ (archiveURL) (liftM nub urllist)

Well, that's that! Let's put it all together:

> import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
> import Text.HTML.Download (openURL)
> import Data.List (isPrefixOf, nub)
> import Monad (liftM)

> main :: IO ()
> main = do articlelist <- liftM words $ getContents
>           urllist <- liftM concat $ mapM fetchArticleURLs articlelist
>           mapM_ (archiveURL) (liftM nub urllist)

> archiveURL :: String -> IO String
> archiveURL url = openURL("http://www.webcitation.org/archive?url=" ++ url ++ "&email=foo@bar.com")

> fetchArticleURLs :: String -> IO [String]
> fetchArticleURLs article = liftM extractURLs $ openURL("http://en.wikipedia.org/wiki/" ++ article)

> extractURLs :: String -> [String]
> extractURLs arg = [x | TagOpen "a" atts <- (parseTags arg), (_,x) <- atts, "http://" `isPrefixOf` x]

Is this not lovely? Not counting imports or type signatures, it's 6 lines (10 with imports, 14 all told) all in clear natural Haskell.

But alas, we see an all too common problem with beautiful concise Haskell: it doesn't perform well. We use it from the shell like thus:

> $ head -n 20 enwiki-all-titles-in-ns0 | archive-bot

It performs acceptably for small values of n, but if we try 100 or more, we quickly notice extreme slowness and outsized memory usage. Looking through the code, 'nub' immediately comes to mind as a culprit. Everywhere else, we're using maps and the like, but nub operates on the whole list of URLs at once, so perhaps it's slow. nub apparently has O(n^2) because it compares every member against every other entry in a list. Let's optimize it. Nub has to do the n^2 matches because it's trying to preserve order (which is sort of important for a list). But we don't care at all about order, so we're free to do something clever... like convert the list to the mathematical structure of sets and then back! We know in sets each member is unique (no {1,1,2} business), so we know it works, and the performance is better than nub. So add in:

> import Data.Set (toList, fromList)

And replace the last line of main with:

>           mapM_ archiveURL $ liftM (toList . fromList) urllist

But we must continue our quest for it is still not fast enough.

The conventional wisdom in the Haskell community seems to be that whenever you are doing String programming and it isn't fast enough, to just use the ByteString library. Our next version will make use of lazy ByteStrings <http://www.cse.unsw.edu.au/~dons/fps.html>:

> import qualified Data.ByteString.Lazy.Char8 as B (ByteString(), getContents, lines, unlines, pack, unpack, words)

To convert to ByteString, we first convert to ByteString equivalents wherever possible - B.words $ B.getContents, for example. Unfortunately for our main function, ByteString introduces some weirdness, so we de-sugar the do notation to use the good old =<< operator to make sure everything fits together. But take heart, it's shorter than before:

> main = do mapM_ archiveURL =<< (liftM uniq $ mapM fetchArticleURLs =<< (liftM B.words $ B.getContents))
>               where uniq :: (Ord a) => [[a]] -> [a] -- Just to be neat; makes clear not ByteString specific
>                     uniq = toList $ fromList $ concat

Notice how we've split out our concatenating and uniqueifying code to its own little points-free local definition. You may've also noticed that we're using the regular concat and not the ByteString concat; this is because we actually aren't concatenating the ByteStrings themselves but merely the list of lists.

The other bit of trickiness is that TagSoup regular Strings, so we need manually convert back and forth between Strings and ByteStrings using B.pack and B.unpack, and of course the type signatures need to be updated. But this is all pretty simple and mechanical; the final result of all this fiddling is the noticeably uglier but also noticeably faster and leaner Archive-bot.hs:

> module Main () where
> import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
> import Text.HTML.Download (openURL)
> import Data.List (isPrefixOf)
> import Monad (liftM)
> import Data.Set (toList, fromList)
> import qualified Data.ByteString.Lazy.Char8 as B (ByteString(), getContents, lines, unlines, pack, unpack, words)

> main :: IO ()
> main = do mapM_ archiveURL =<< (liftM uniq $ mapM fetchArticleURLs =<< (liftM B.words $ B.getContents))
>               where uniq :: (Ord a) => [[a]] -> [a]
>                     uniq = toList $ fromList $ concat

> archiveURL :: B.ByteString -> IO String
> archiveURL url = openURL("www.webcitation.org/archive?url=" ++ (B.unpack url) ++ "&email=foo@bar.com")

> fetchArticleURLs :: B.ByteString -> IO [B.ByteString]
> fetchArticleURLs article = liftM extractURLs $ openURL("en.wikipedia.org/wiki/" ++ (B.unpack article))

> extractURLs :: String -> [B.ByteString]
> extractURLs arg = map B.pack $ [x | TagOpen "a" atts <- (parseTags arg), (_,x) <- atts, "http://" `isPrefixOf` x]

What's that, you say? While ByteStrings has indeed made some difference, it's marginal and not even 10%? Fortunately, we can go still further - there's not a whole lot we can do for the algorithm itself yet, but we can investigate parallelizing matters. Remember me mentioning that archiveURL was a bit inefficient because even with mapM_, each entry was being processed one at a time? Well, is this not Haskell? Aren't functional programming languages famous for having functions like map which are embarrassingly easy to parallelize? If archiveURL were pure, we'd use Control.Parallel.Strategies, but it's not. Remember that we're throwing away the output from openURL inside archiveURL. So ideally we would somehow be mapping a function over urllist which would say in effect "Take this single ByteString and go off somewhere by yourself and do whatever needs doing; don't bother coming back." In short, we want to fork archiveURL off, and we want forkIO.

Import forkIO and change the mapM_ to use it:

> import Control.Concurrent (forkIO)
> main = do mapM_ (forkIO . archiveURL) =<< (liftM uniq $ mapM fetchArticleURLs =<< (liftM B.words $ B.getContents))
> -- We need return () because otherwise it'd return a String and break forkIO's heart
> archiveURL url = do openURL("www.webcitation.org/archive?url=" ++ (B.unpack url) ++ "&email=foo@bar.com"); return ();

Much better! I've noticed improvements of between 20 to 100 percent, so that little change was certainly worth it. But these speed increases have made me greedy - I want to be able to process a few thousand names at once, not a measly few hundred. This should cause us to ponder. Don Stewart has a quite fast program to check the viability of links <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/urlcheck-0.1>; it delves pretty heavily into threads and gets great performance but it honestly looks pretty scary and hard to read. Do we have to resort to such nastiness?

Well, there's a saying I've seen: "Make it lazier or make it stricter." It's already strict in a lot of places because of the IO, and going further doesn't seem like a good option - how can one pipe in thousands of articles if the program is going to strictly and immediately eat all one's RAM? Laziness is the way to go. To figure out how to get greater laziness, let's go through step by step. We know getContents is lazy because it makes use of unsafeInterleaveIO according to the documentation - which is why we can have lazy IO. Let's hold onto that thought. Next is B.words. A quick experiment in GHCi (take 100 $ unwords $ repeat "Yes "; that sort of thing) convinces me that words is lazy as well. Next is 'mapM fetchArticleURLs'. Here's a problem. fetchArticleURLs does IO and so is strict, and mapM means it is little better than a loop over the list - the entire list. There goes our laziness since the whole list will be processed before being passed onto forkIO and archiveURL. But laziness and IO can co-exist in peace and harmony!

Remember getContents is lazy, and it can achieve this magic using 'unsafeInterleaveIO'. Reading through the documentation, it seems to be safe in certain situations, like when the IO target isn't going to change, isn't going to change as a result of whatever is done - or as with us, whether we don't care when the IO is performed. In our situation, it's irrelevant whether we access an article at time n or time n+1. We don't care about whatever changes between then, even if it's something as drastic as the article being deleted (hey, we have 1.7 million other articles to cover). So we can safely use unsafeInterleaveIO! We could stick it in main or fetchArticleURLs, but for convenience's sake, I put it in the latter.

So now we have:

> import System.IO.Unsafe (unsafeInterleaveIO) -- Scary name, but it's safe and useful for us
> fetchArticleURLs article = liftM (B.lines . extractURLs) (unsafeInterleaveIO $ openURL(wiki ++ B.unpack article))

Let's think about the flow now. getContents keeps on delivering a few characters to words, which is waiting for a newline, at which point it turns those characters into a string and passes them onto mapM which will immediately run fetchArticleURLs - and fetchArticleURLs will immediately 'finish', leaving behind some thunks or whatever holding a promise that it'll run the IO actions (downloading a particular Web page) whenever it's needed. And that promise gets passed into uniq, which is busy concatenating and then uniqueifying the results of those thunks. So now the program will basically run in constant space (I notice that even when I pass it 10000 or 20000 names it'll use a steady 7.6% or so of memory as opposed to a varying ~40% with previous versions running on a few hundred names). It may take a while longer, but this is a small price to pay - now my computer will still be usable while the program is running.

Are we tired of this program yet? I've thought of one last and particularly nasty thing to do to speed things up and recover the speed lost to laziness. We have uniq in there to remove duplicate entries and be a little friendlier to WebCite, correct? But if the program is hardwired to always run on en.wikipedia.org pages, then we can just run fetchArticleURLs on a few pages, note down the duplicates, and hard-wire in a filter to remove them. As I said, this is truly terrible and if I thought anyone else might want to use this program on any other website, I would've never contemplated it.

But here is the final, ugly, fast and frugal version. This combines the lazy IO covered previously and adds in the hardwired filter.

> module Main () where
>
> import Control.Concurrent (forkIO)
> import Control.Monad (liftM)
> import Data.List (isPrefixOf)
> import System.IO.Unsafe (unsafeInterleaveIO)
> import Text.HTML.Download (openURL)
> import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
> import qualified Data.ByteString.Lazy.Char8 as B (ByteString(), getContents, pack, unpack, words)

> main :: IO ()
> main = mapM_ (forkIO . archiveURL) =<< (liftM uniq $ mapM fetchArticleURLs =<< (liftM B.words $ B.getContents))
>                 where uniq :: [[B.ByteString]] -> [B.ByteString]
>                       uniq = filter (\x -> if x == B.pack "http://wikimediafoundation.org/" || x == B.pack "http://wikimediafoundation.org/wiki/Deductibility_of_donations" || x == B.pack "http://wikimediafoundation.org/wiki/Fundraising" || x == B.pack "http://wikimediafoundation.org/wiki/Privacy_policy" || x == B.pack "http://www.mediawiki.org/" || x == B.pack "http://www.wikimediafoundation.org" then False else True) . concat

> fetchArticleURLs :: B.ByteString -> IO [B.ByteString]
> fetchArticleURLs article = liftM extractURLs $ unsafeInterleaveIO $ openURL("http://en.wikipedia.org/wiki/" ++ B.unpack article)

> extractURLs :: String -> [B.ByteString]
> extractURLs arg = map B.pack $ [x | TagOpen "a" atts <- (parseTags arg), (_,x) <- atts, "http://" `isPrefixOf` x]

> archiveURL :: B.ByteString -> IO ()
> archiveURL url = openURL("www.webcitation.org/archive?url=" ++ (B.unpack url) ++ "&email=foo@bar.com") >>
 return ();

# See also
- [Wikipedia RSS Archive Bot.lhs]() -(the next step)