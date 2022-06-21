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
