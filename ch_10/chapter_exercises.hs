module ChapterExercises where

    -- 1

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
