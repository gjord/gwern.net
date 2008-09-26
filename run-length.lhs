Recently I was playing with and working on a clone of the old Gradius arcade games which was written in Haskell. Most of my changes were not particularly interesting (cleaning up, Cabalizing, fixing warnings, switching all Integers to Ints and so on), but in its Demo.hs, I found an interesting solution to an interesting problem which seems to be a good example of how Haskell's abstractions can really shine.

Suppose we have levels which are specified by a pair of numbers and then a long list of numbers, often very repetitious. Perhaps a particular level might be represented this way:

> level1 = ((Int,Int),[Int])
> level1 = ((2,1),[0,0,0,0,0,0,0,0,0,0,0,0,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,73,73,73,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,69,69,69,69,69,69,65,17,17,17,17,17,17,17,17,17,17,17,17,17,25,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,65,65,65,65,65,65,1,1,1,1,1,1,1,1,33,33,1,1,1,1,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,49,49,49,49,33,33,33,1,1,1,1,1,1,1,9,9,9,1,1,1,1,1,1,1,1,1,33,33,33,33,1,1,1,1,1,1,1,1,1,9,9,1,1,1,1,1,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,1,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,81,81,81,81,81,81,81,81,81,81,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,1,1,1,1,1,1,1,17,17,17,17,17,17,17,17,17,17,17,1,1,1,1,1,1,1,1,1,1,1,1,17,17,17,1,1,1,1,1,1,1,1,1,1,1,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,97,33,33,33,33,37,5,69,69,65,65,65,65,65,65,67,67,67,67,67,67,67,67,67,75,75,75,75,75,75,75,75,75,75,75,75,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,3,3,3,3,3,3,3,3,3,3,3,3,3,11,11,3,3,3,3,3,3,3,3,3,3,11,3,3,3,3,3,3,3,3,3,3,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,3,3,3,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,3,3,3,3,3,3,3,67,67,67,67,67,67,3,3,3,67,67,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,3,3,3,3,3,3,67,67,67,67,67,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,11,11,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,35,35,35,35,35,3,3,3,35,35,35,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,35,35,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,19,19,19,19,19,19,19,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,11,11,11,11,11,43,43,43,43,43,43,43,43,35,35,35,35,35,35,35,35,3,3,3,35,35,35,35,35,35,35,35,35,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,35,35,35,35,35,35,35,35,35,35,35,35,35,3,3,3,35,35,35,35,35,35,35,35,35,35,35,3,3,3,3,3,35,35,35,35,35,35,35,35,35,3,3,3,3,35,35,35,35,35,35,3,3,3,3,35,35,35,35,35,3,3,3,3,3,3,3,3,3,3,3,3,3,19,19,19,19,19,19,19,19,19,19,19,19,19,19,3,3,3,3,19,19,19,19,3,3,3,3,3,3,19,19,19,19,3,3,3,3,3,3,3,19,19,19,19,19,3,3,3,3,3,3,3,3,19,19,19,19,19,19,19,19,83,83,83,83,83,67,67,67,3,3,19,19,19,19,19,19,19,19,19,19,19,19,83,83,83,83,83,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,83,83,83,83,83,83,83,83,83,19,19,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,75,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,43,35,35,35,35,3,3,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,3,3,19,19,19,19,19,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,19,19,19,19,19,19,3,3,3,3,3,3,3,3,67,67,67,67,67,67,83,83,83,83,83,83,83,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,19,19,19,19,19,19,19,19,19,51,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,35,35,35,35,35,35,35,35,35,35,35,35,35,35,3,3,3,3,3,3,3,35,35,43,43,43,43,43,43,43,43,43,43,43,11,11,11,11,11,11,3,3,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,3,3,3,3,3,3,3,35,35,35,35,35,35,35,35,35,35,35,35,35,43,43,43,43,43,11,11,11,11,11,11,11,3,3,67,67,67,67,67,83,19,19,3,3,67,67,67,67,67,67,67,67,67,67,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])

This is clearly a bad way of representing things. We could just scrap this representation as Ints completely and perhaps define it as

>  -- Assume that these datatypes are already defined and everything
> level1 :: ((Geometry,Geometry),[Enemies])
> level1 = ((Tall,Narrow), [FlyEnemy,FlyEnemy,FlyEnemy,FlyEnemy,FlyEnemy,FlyEnemy,Shooter,PowerUp,Boss..])

But this representation, while certainly more symbolic, is still very repetitious. We need some way of expressing this more concisely, of, in short, *compressing* it. In this vein of thought, our first observation should be that we do not need to resort to Gzipping it or adding dependencies on fancy compression libraries or anything; there is a very obvious way to compress it already - the entire thing is practically just a series of repeated numbers. We should be able to replace the length enumeration of [0,0,0,0,0,0,0,0,0,0,0,0] with something simpler, say the number of repetition and what is to be repeated so that our entry would look like (12,0). This representation is definitely shorter and more importantly, easier to modify and not a constant. It is possible that there may be a performance benefit here, as we've gotten rid of a large constant that would have to be defined in the program itself and instead replaced it with a shorter function which evaluates to the same thing.

So, what is the type of our decompressing function? Well, we need to turn a (Int,Int) into a [Int]; even better, we want to turn a whole list of (Int,Int)s into a single list of Ints. Thus, our end goal is going to be a function of this type:

> rleDecode :: [(Int,Int)] -> [Int]

Let us tackle the single tuple example first. The second entry defines what we need, and the first entry defines how many we need. We could write a recursive function that takes the parameter, decreases the first entry by one, and cons on one example of the second entry. It could look like this:

> rleRecursiveDecode :: (Int,Int) -> [Int]
> rleRecursiveDecode (0,_) = []
> rleRecursiveDecode (n,b) = b : (rleRecursiveDecode (n-1,b))

But this really is not the best way to go. It is not necessarily easy to follow, and if there is one thing I have learned about Haskell programming, it is that the most obvious approach (in this case, primitive recursion) may not be the best way to go. This is code that could have as well been written in Scheme or something. It is complicated because we are trying to ensure that we generate an item for the list only at the exact moment we need to add it into the list; we are programming as if our language is strict, in other words.

So, what is the lazy way of doing things? Infinite lists, of course. We create an infinite list containing only the second entry, and we merely take as many as the first entry says we need. Simple enough!

> -- Infinite list of our item of interest.
> reduplicate a = a:a

> rleLazyDecode :: (Int,Int) -> [Int]
> rleLazyDecode (n,b) = take n (reduplicate b)

Now, reduplicate is a simple enough function to define, but it already has a definition in the standard libraries - 'cycle'. (I assume you know what 'take' is, but that is also fairly easy to define once you seen the need for it.) So:

> rleLazyDecode (n,b) = take n (cycle b)

Might as well remove the parentheses:

> rleLazyDecode (n,b) = take n $ cycle b

A satisfying, short, functional, and lazy one-liner. From here the definition of rleDecode is almost trivial: we extend it to a list of tuples by throwing in a map, and we turn the resulting list of lists into a single list by way of 'concat':

> rleDecode ns = concat $ map rleLazyDecode ns

We can tweak this further, as 'concat . map' is a common enough idiom that there is a shortcut:

> rleDecode ns = concatMap rleLazyDecode ns

Aw heck - make it points-free:

> rleDecode = concatMap rleLazyDecode

And substitute in the rleLazyDecode definition:

> rleDecode = concatMap (\(n,b) -> take n $ cycle b)

We could also write a version that omits the explicit lambda and naming of parameters by use of the helpful but somewhat esoteric 'uncurry' function; Uncurry takes apart the tuple. Tts type is:

> uncurry :: (a -> b -> c) -> (a, b) -> c

We can actually go even further into the realms of incomprehensibility. It turns out that lists are counter-intuitively a monad! This means we can use bind and all the rest of the operations defined by the Monad typeclass to operate on lists and other things. So we can write the bizarre (but short and effective) version of rleDecode that follows:

> rleDecode = (uncurry replicate =<<)

And we are done! We can now represent the first list like this:

> d1 = ((2,1),d)
>     where d = rleDecode [(5, 3), (10, 67), (6, 3), (5, 67), (29, 3), (2, 11), (29, 3), (5, 35), (3, 3), (3, 35), (24, 3), (2, 35), (19, 3), (7, 19), (21, 51), (63, 35), (15, 43), (5, 11), (8, 43), (8, 35), (3, 3), (9, 35), (20, 3), (52, 67), (32, 3), (13, 35), (3, 3), (11, 35), (5, 3), (9, 35), (4, 3), (6, 35), (4, 3), (5, 35), (13, 3), (14, 19), (4, 3), (4, 19), (6, 3), (4, 19), (7, 3), (5, 19), (8, 3), (8, 19), (5, 83), (3, 67), (2, 3), (12, 19), (5, 83), (17, 19), (9, 83), (2, 19), (5, 3), (20, 67), (1, 75), (38, 11), (1, 43), (4, 35), (7, 3), (57, 67), (2, 3), (5, 19), (17, 3), (6, 19), (8, 3), (6, 67), (7, 83), (59, 3), (9, 19), (1, 51), (17, 35), (20, 3), (14, 35), (7, 3), (2, 35), (11, 43), (6, 11), (7, 3), (26, 67), (7, 3), (13, 35), (5, 43), (7, 11), (2, 3), (5, 67), (1, 83), (2, 19), (2, 3), (10, 67), (21, 3), (147, 0)]

Much nicer, don't you think? And the best part is, we should be able to reuse this run-length decoding even if we replace the arbitrary numbers by more descriptive type constructors!

(Many thanks to the good denizens of #haskell, and Don Stewart's blog entry on run-length encoding/decoding using Arrows <http://cgi.cse.unsw.edu.au/~dons/blog/2007/07>.)
