module ChapterExercises where

import Cipher
import Data.Char
import Data.List (intercalate)

{-
Multiple Choice

1. a
2. c
3. b
4. c
-}

-- Ciphers

getShiftAmount :: Char -> Int
getShiftAmount letter
  | isUpper letter = ord letter - 65
  | isLower letter = ord letter - 97
  | otherwise = 0

repeatKeywordForMessageLength :: String -> String -> String
repeatKeywordForMessageLength keyword message =
  take (length $ concat $ words message) $ concat $ repeat keyword

encodeSecondLetterBasedOnFirst :: Char -> Char -> Char
encodeSecondLetterBasedOnFirst key = shiftLetter (getShiftAmount key)

decodeSecondLetterBasedOnFirst :: Char -> Char -> Char
decodeSecondLetterBasedOnFirst key = shiftLetter (negate $ getShiftAmount key)

addSpacesToRepeatedKeyword [] _ = []
addSpacesToRepeatedKeyword _ [] = []
addSpacesToRepeatedKeyword (a : as) (b : bs) =
  if b == ' '
    then ' ' : addSpacesToRepeatedKeyword (a : as) bs
    else a : addSpacesToRepeatedKeyword as bs

vigenere :: String -> String -> String
vigenere key message = zipWith encodeSecondLetterBasedOnFirst (addSpacesToRepeatedKeyword (repeatKeywordForMessageLength key message) message) message

unVigenere :: String -> String -> String
unVigenere key message = zipWith decodeSecondLetterBasedOnFirst (addSpacesToRepeatedKeyword (repeatKeywordForMessageLength key message) message) message

-- as patterns

-- 1.

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf x@(a : as) (b : bs)
  | a == b = isSubseqOf as bs
  | otherwise = isSubseqOf x bs

-- 2.

capitalizeWords :: String -> [(String, String)]
capitalizeWords message = map pairWordWithCapitalizedWord $ words message

pairWordWithCapitalizedWord :: String -> (String, String)
pairWordWithCapitalizedWord word@(x : xs) = (word, toUpper x : xs)

-- Language Exercises

-- 1.
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs)
  | x /= ' ' = toUpper x : xs
  | otherwise = x : capitalizeWord xs

-- 2.

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

-- should be called capitalizeSentence IMO
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph sentence = intercalate "." $ map capitalizeWord $ wordsWhen (== '.') sentence