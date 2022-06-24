module ChapterExercises where
import Data.Function ( on )
-- 1

stops :: [Char]
stops = "pbtdkg"
vowels = "aeiou"

-- 1a 

threeTuples :: [a] -> [b] -> [(a, b, a)]
threeTuples as bs = [(x,y,z) | x <- as, y <- bs, z <- as]

stopVowelStop = threeTuples stops vowels

-- 1b

startsWithP :: (Char, b, c) -> Bool
startsWithP ('p', _, _) = True
startsWithP _ = False

theTuplesStartingWithP = [tuples | tuples <- stopVowelStop, startsWithP tuples]

-- 1c

nouns = ["tree", "man", "dog", "there"]
verbs =["walked", "ran", "is", "twirled"]

nounVerbNoun = threeTuples nouns verbs

-- 2 

-- div (sum (map length (words x))) (length (words x))

-- divide sum of the number of letters in words by the number of words there are
-- ie average length of word in string
-- (after testing, caveat is that because of div, is lowest whole number result)

-- 3 

avgWordLen x = (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))

-- Rewriting functions using folds

-- 1
-- direct recursion, not using (||)
myOr1 :: [Bool] -> Bool
myOr1 [] = False
myOr1 (x:xs) = if x == True then True else myOr1 xs
-- direct recursion, using (||)
myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (x:xs) = x || myOr2 xs
-- fold, not point-free in folding function
myOr3 :: [Bool] -> Bool
myOr3 = foldr (\a b -> if a then True else b) False
-- fold, point-free
myOr4 :: [Bool] -> Bool
myOr4 = foldr (||) False

-- 2 
-- direct recursion not using (||)
myAny1 :: (a -> Bool) -> [a] -> Bool
myAny1 f [] = False 
myAny1 f (x:xs) = if f x then True else myAny1 f xs
-- direct recursion using (||)
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f [] = False 
myAny2 f (x:xs) = f x || myAny2 f xs
-- fold, not point-free in folding function
myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f = foldr (\a b -> if f a then True else b) False
-- fold, point-free in folding function
myAny4 :: (a -> Bool) -> [a] -> Bool
myAny4 f = foldr ((||) . f) False
{- ^This took me some time to work out (and some time on IRC channel)
    reducing lambda to point free form
    z = \x y -> (||) (f x) y
    z = \x-> (||) (f x)
    z = (||) . f
-}
-- 3
-- direct recursion 
myElem1 :: Eq a => a -> [a] -> Bool
myElem1 y [] = False
myElem1 y (x:xs) = if y == x then True else myElem1 y xs
-- direct recursion using (||)
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 y [] = False
myElem2 y (x:xs) = y == x || myElem1 y xs
-- fold, not point-free in folding function
myElem3 :: Eq a => a -> [a] -> Bool
myElem3 y = foldr (\a b-> if y==a then True else b) False
-- fold, point free in folding function
myElem4 :: Eq a => a -> [a] -> Bool
myElem4 y = foldr ((||) . (y==)) False
-- use any
myElem5 :: Eq a => a -> [a] -> Bool
myElem5 y = any (y==)
-- 4
-- direct recursion
myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 (x:xs) = myReverse1 xs ++ [x]
-- using fold
myReverse2 :: [a] -> [a]
myReverse2 = foldr (\x -> (++[x])) []
-- 5
-- direct recursion
myMap1 :: (a -> b) -> [a] -> [b]
myMap1 f [] = []
myMap1 f (x : xs) = f x : myMap1 f xs
-- using fold
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f = foldr (\a b -> f a : b) []
-- using fold w point free fold function
myMap3 :: (a -> b) -> [a] -> [b]
myMap3 f = foldr ((:) . f) []
-- 6
-- using recursion
myFilter1 :: (a -> Bool) -> [a] -> [a]
myFilter1 f [] = []
myFilter1 f (x:xs) = if f x then x : myFilter1 f xs else myFilter1 f xs
-- using fold
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 f = foldr (\a b -> if f a then a : b else b) []
-- 7
-- recursive
squish1 :: [[a]] -> [a]
squish1 [] = []
squish1 (x:xs) = x ++ squish1 xs
-- using fold
squish2 :: [[a]] -> [a]
squish2 = foldr (\x y -> x ++ y) []
-- using fold point free
squish3 :: [[a]] -> [a]
squish3 = foldr (++) []
-- 8
-- recursive
squishMap1 :: (a -> [b]) -> [a] -> [b]
squishMap1 f [] = []
squishMap1 f (x:xs) = f x ++ squishMap1 f xs
-- using foldr
squishMap2 :: (a -> [b]) -> [a] -> [b]
squishMap2 f = foldr (\a b -> f a ++ b) []
-- using foldr w point free folding function
squishMap3 :: (a -> [b]) -> [a] -> [b]
squishMap3 f = foldr ((++) . f) []
-- 9 
-- can use any squishMaps, pretty trivial
squishAgain :: [[a]] -> [a]
squishAgain = squishMap3 id 
-- 10
-- recursive
myMaximumBy1 :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy1 f (x : xs) = go f (x : xs) x
  where
    go f [] max = max
    go f (x : xs) max = if f x max == GT then go f xs x else go f xs max
-- using foldr
myMaximumBy2 :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy2 f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x (x:xs)
-- 11
-- recursive
myMinimumBy1 :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy1 f (x : xs) = go f (x : xs) x
  where
    go f [] max = max
    go f (x : xs) max = if f x max == LT then go f xs x else go f xs max
-- using foldr
myMinimumBy2 :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy2 f (x:xs) = foldr (\a b -> if f a b == LT then a else b) x (x:xs)