module Utils where

import Data.List (intercalate)
import HUnitJudge (isEqual)
import Test.HUnit (Test(TestCase))

sjoin :: [String] -> String
sjoin = intercalate " "

generateTest1 :: (Show t, Show a, Eq a)
              => [Char]
              -> (t -> a)
              -> (t -> a)
              -> [t]
              -> [Test]
generateTest1 msg fs fi input = [ TestCase $ isEqual (sjoin [msg, show x]) (fs x) (fi x)
                                | x <- input ]

generateTest2 :: (Show t1, Show t, Show a, Eq a)
              => [Char]
              -> (t -> t1 -> a)
              -> (t -> t1 -> a)
              -> [(t, t1)]
              -> [Test]
generateTest2 msg fs fi input = [ TestCase $ isEqual (sjoin [msg, show x, show y]) (fs x y) (fi x y)
                                | (x, y) <- input ]

generateInfix :: (Show t1, Show t, Show a, Eq a)
              => [Char]
              -> (t -> t1 -> a)
              -> (t -> t1 -> a)
              -> [(t, t1)]
              -> [Test]
generateInfix msg fs fi input = [ TestCase $ isEqual (sjoin [show x, msg, show y]) (fs x y) (fi x y)
                                | (x, y) <- input ]

generateTest3 :: (Show t2, Show t1, Show t, Show a, Eq a)
              => [Char]
              -> (t -> t1 -> t2 -> a)
              -> (t -> t1 -> t2 -> a)
              -> [(t, t1, t2)]
              -> [Test]
generateTest3 msg fs fi input = [ TestCase $ isEqual (sjoin [msg, show x, show y, show z]) (fs x y z) (fi x y z)
                                | (x, y, z) <- input ]

generateTest4 :: (Show t3, Show t2, Show t1, Show t, Show a, Eq a)
              => [Char]
              -> (t -> t1 -> t2 -> t3 -> a)
              -> (t -> t1 -> t2 -> t3 -> a)
              -> [(t, t1, t2, t3)]
              -> [Test]
generateTest4 msg fs fi input = [ TestCase $ isEqual (sjoin [msg, show x, show y, show z, show q]) (fs x y z q) (fi x y z q)
                                | (x, y, z, q) <- input ]

combos2 :: [a] -> [b] -> [(a, b)]
combos2 xs ys = [ (x, y) | x <- xs, y <- ys ]

combos3 :: [a] -> [b] -> [c] -> [(a, b, c)]
combos3 xs ys zs = [ (x, y, z) | x <- xs, y <- ys, z <- zs ]

combos4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
combos4 ws xs ys zs = [ (w, x, y, z) | w <- ws, x <- xs, y <- ys, z <- zs ]
