Recently I was playing with and working on a Haskell clone of the old [_Gradius_](!Wikipedia "Gradius_(series\")) arcade games, [_Monadius_](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/Monadius). Most of my changes were not particularly interesting (cleaning up, Cabalizing, fixing warnings, switching all <tt>Integer</tt>s to <tt>Int</tt>s and so on), but in its Demo.hs, I found an interesting solution to a problem, and it seems like a good & real example of how Haskell's abstractions can shine.

# Examples

One of the problems with language advocacy is that it's hard to have good examples, since someone who is writing an essay probably will only come up with trivial ones, and someone dealing with meaty problems isn't thinking about advocacy but how to solve their problem. I hope this example will be in-between.

# Level data in _Monadius_

Suppose we have levels which are specified by a pair of numbers and then a long list of numbers, often very repetitious. Perhaps a particular level might be represented this way:

> level1 :: ((Int, Int), [Int])
> level1 = ((2,1),[0,0,0,0,0,0,0,0,0,0,0,0,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
> 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,73,73,73,65,65,65,65,65,65,65,
> 65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,69,69,69,69,
> 69,69,65,17,17,17,17,17,17,17,17,17,17,17,17,17,25,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,9,
> 9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,65,65,65,65,65,65,1,1,1,1,1,1,1,1,33,33,1,1,1,1,33,33,33,33,33,33,33,33,33,
> 33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,49,49,49,49,33,33,33,1,1,1,1,1,1,1,9,9,9,1,1,1,1,1,1,1,1,1,
> 33,33,33,33,1,1,1,1,1,1,1,1,1,9,9,1,1,1,1,1,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,
> 33,33,33,33,1,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,
> 17,17,17,81,81,81,81,81,81,81,81,81,81,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,
> 17,17,17,17,17,17,17,17,17,17,17,17,17,1,1,1,1,1,1,1,17,17,17,17,17,17,17,17,17,17,17,1,1,1,1,1,1,1,1,1,1,
> 1,1,17,17,17,1,1,1,1,1,1,1,1,1,1,1,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,
> 65,65,65,65,65,65,65,65,65,97,33,33,33,33,37,5,69,69,65,65,65,65,65,65,67,67,67,67,67,67,67,67,67,75,75,75,
> 75,75,75,75,75,75,75,75,75,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
> 11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
> 11,11,11,11,11,11,11,11,11,11,11,11,3,3,3,3,3,3,3,3,3,3,3,3,3,11,11,3,3,3,3,3,3,3,3,3,3,11,3,3,3,3,3,3,3,3,3,
> 3,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,3,3,3,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,67,
> 67,67,67,67,67,3,3,3,3,3,3,3,67,67,67,67,67,67,3,3,3,67,67,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,3,3,3,3,
> 3,3,67,67,67,67,67,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,11,11,3,3,3,3,3,3,3,3,3,3,3,3
> ,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,35,35,35,35,35,3,3,3,35,35,35,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
> 3,3,3,35,35,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,19,19,19,19,19,19,19,51,51,51,51,51,51,51,51,51,51,51,51,
> 51,51,51,51,51,51,51,51,51,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,
> 35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,
> 43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,11,11,11,11,11,43,43,43,43,43,43,43,43,35,35,35,35,35,35,35,35,
> 3,3,3,35,35,35,35,35,35,35,35,35,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,67,67,
> 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,
> 67,67,67,67,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,35,35,35,35,35,35,35,35,35,35,35,
> 35,35,3,3,3,35,35,35,35,35,35,35,35,35,35,35,3,3,3,3,3,35,35,35,35,35,35,35,35,35,3,3,3,3,35,35,35,35,35,35,3
> ,3,3,3,35,35,35,35,35,3,3,3,3,3,3,3,3,3,3,3,3,3,19,19,19,19,19,19,19,19,19,19,19,19,19,19,3,3,3,3,19,19,19,19,
> 3,3,3,3,3,3,19,19,19,19,3,3,3,3,3,3,3,19,19,19,19,19,3,3,3,3,3,3,3,3,19,19,19,19,19,19,19,19,83,83,83,83,83,67,
> 67,67,3,3,19,19,19,19,19,19,19,19,19,19,19,19,83,83,83,83,83,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
> ,19,83,83,83,83,83,83,83,83,83,19,19,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,75,
> 11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11
> ,11,43,35,35,35,35,3,3,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67
> ,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,3,3,19,19,19,19,
> 19,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,19,19,19,19,19,19,3,3,3,3,3,3,3,3,67,67,67,67,67,67,83,83,83,83,83,83,83,
> 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
> 3,3,3,3,19,19,19,19,19,19,19,19,19,51,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,3,3,3,3,3,3,3,3,3,3,3,
> 3,3,3,3,3,3,3,3,3,35,35,35,35,35,35,35,35,35,35,35,35,35,35,3,3,3,3,3,3,3,35,35,43,43,43,43,43,43,43,43,43,43,
> 43,11,11,11,11,11,11,3,3,3,3,3,3,3,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,
> 67,3,3,3,3,3,3,3,35,35,35,35,35,35,35,35,35,35,35,35,35,43,43,43,43,43,11,11,11,11,11,11,11,3,3,67,67,67,67,67
> ,83,19,19,3,3,67,67,67,67,67,67,67,67,67,67,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,
> 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
> 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
>  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])

This is clearly a bad way of representing things. We could just scrap this representation as <tt>Int</tt>s completely and perhaps define it as

> level1 :: ((Geometry,Geometry),[Enemy])
> level1 = ((Tall,Narrow), [FlyEnemy,FlyEnemy,FlyEnemy,FlyEnemy,FlyEnemy,FlyEnemy,Shooter,PowerUp,Boss..])

But this representation, while certainly more symbolic, is still very repetitious. It will, in fact, take even more space to write down. We could have the <tt>Read</tt> and <tt>Show</tt> typeclasses auto-derived for our <tt>Geometry</tt> and <tt>Enemy</tt> datatypes, and then we could ship <tt>level1</tt>, <tt>level2</tt> etc. as some sort of "levelinformation.dat" file, and read the level information in at runtime. But that makes installation and running more difficult, and it doesn't address the core issue: the information is written in a way that is so ungainly that it's next to impossible to write or even *modify*!

# The solution

We need some way of expressing this more concisely, of, in short, *compressing* it. In this vein of thought, our first observation should be that we do not need to resort to Gzipping it or adding dependencies on fancy compression libraries or anything; there is a very obvious way to compress it already - the entire thing is practically just a series of repeated numbers.

We should be able to replace the length enumeration of <tt>[0,0,0,0,0,0,0,0,0,0,0,0]</tt> with something simpler, like the number of repetitions, and what is to be repeated. Our entry would look like <tt>(12,0)</tt>.

This representation is definitely shorter and more importantly, easier to modify. It is possible that there may be a performance benefit here, as we've gotten rid of a large constant that would have to be defined in the program itself and instead replaced it with a shorter function which evaluates to the same thing. (If we're only on level 1, we don't need to carry around the expanded version of the other levels, and when we go to level 2, level 1 will be garbage-collected.)

## Writing it

So, what is the type of our decompressing function? Well, we need to turn a <tt>(Int,Int)</tt> into a <tt>[Int]</tt>; even better, we want to turn a whole list of <tt>(Int,Int)</tt>s into a single list of <tt>Int</tt>s. Thus, our end goal is going to be a function of this type:

> rleDecode :: [(Int,Int)] -> [Int]

Let us tackle the single tuple example first. The second entry defines what we need, and the first entry defines how many we need. We could write a recursive function that takes the parameter, decreases the first entry by one, and cons on one example of the second entry. It could look like this:

> rleRecursiveDecode (0,_) = []
> rleRecursiveDecode (n,b) = b : (rleRecursiveDecode (n-1,b))

But this really is not the best way to go. It is not necessarily easy to follow, and if there is one thing I have learned about Haskell programming, it is that the most obvious approach (in this case, primitive recursion) may not be the best way to go. This is code that could have as well been written in Scheme or something. It is complicated because we are trying to ensure that we generate an item for the list only at the exact moment we need to add it into the list; we are programming as if our language is strict, in other words.

So, what is the lazy way of doing things? Infinite lists, of course. We create an infinite list containing only the second entry, and we merely take as many as the first entry says we need. Simple enough!

> -- Define an infinite list of our item.
> reduplicate a = a:a

> rleLazyDecode :: (Int,Int) -> [Int]
> rleLazyDecode (n,b) = take n (reduplicate b)

Now, <tt>reduplicate</tt> is a simple enough function to define, but it already has a definition in the standard libraries - <tt>cycle</tt>. (I assume you know what <tt>take</tt> is, but that is also fairly easy to define once you seen the need for it.)

So:

> rleLazyDecode (n,b) = take n (cycle b)

Might as well remove the parentheses:

> rleLazyDecode (n,b) = take n $ cycle b

A satisfying, short, functional, and lazy one-liner. From here the definition of rleDecode is almost trivial: we extend it to a list of tuples by throwing in a <tt>map</tt>, and we turn the resulting list of lists into a single list by way of <tt>concat</tt>:

> rleDecode ns = concat $ map rleLazyDecode ns

We can tweak this further, as <tt>'concat . map'</tt> is a common enough idiom that there is a shortcut:

> rleDecode ns = concatMap rleLazyDecode ns

Aw heck - let's make it point-free:

> rleDecode = concatMap rleLazyDecode

And then we substitute in the <tt>rleLazyDecode</tt> definition:

> rleDecode = concatMap (\(n,b) -> take n $ cycle b)

We could also write a version that omits the explicit lambda and naming of parameters by use of the helpful (but somewhat esoteric) <tt>uncurry</tt> function; <tt>uncurry</tt> takes apart the tuple. Its type is:

> uncurry :: (a -> b -> c) -> (a, b) -> c

We can actually go even further into the realms of incomprehensibility. It turns out that lists are a monad! This means we can use <tt>bind</tt> and all the rest of the operations defined by the <tt>Monad</tt> typeclass to operate on lists and other things. So we can write this bizarre (but short and effective) version of rleDecode:

> rleDecode = (uncurry replicate =<<)

And we are done! We can now represent the first level like this:

> d1 = ((2,1),d)
>     where d = rleDecode [(5, 3), (10, 67), (6, 3), (5, 67), (29, 3), (2, 11),
>      (29, 3), (5, 35), (3, 3), (3, 35), (24, 3), (2, 35), (19, 3), (7, 19), (21, 51),
>      (63, 35), (15, 43), (5, 11), (8, 43), (8, 35), (3, 3), (9, 35), (20, 3), (52,
>      67), (32, 3), (13, 35), (3, 3), (11, 35), (5, 3), (9, 35), (4, 3), (6, 35), (4,
>      3), (5, 35), (13, 3), (14, 19), (4, 3), (4, 19), (6, 3), (4, 19), (7, 3), (5,
>      19), (8, 3), (8, 19), (5, 83), (3, 67), (2, 3), (12, 19), (5, 83), (17, 19), (9,
>      83), (2, 19), (5, 3), (20, 67), (1, 75), (38, 11), (1, 43), (4, 35), (7, 3),
>      (57, 67), (2, 3), (5, 19), (17, 3), (6, 19), (8, 3), (6, 67), (7, 83), (59, 3),
>      (9, 19), (1, 51), (17, 35), (20, 3), (14, 35), (7, 3), (2, 35), (11, 43), (6,
>      11), (7, 3), (26, 67), (7, 3), (13, 35), (5, 43), (7, 11), (2, 3), (5, 67), (1,
>      83), (2, 19), (2, 3), (10, 67), (21, 3), (147, 0)]

Much nicer, don't you think? And the best part is, we should be able to reuse this run-length decoding even if we replace the arbitrary numbers by more descriptive type constructors!

(My thanks to the good denizens of #haskell, and [Don Stewart's blog entry](http://cgi.cse.unsw.edu.au/~dons/blog/2007/07) on run-length encoding/decoding using Arrows.)
