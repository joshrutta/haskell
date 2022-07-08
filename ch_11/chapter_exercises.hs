{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module ChapterExercises where

import Cipher
import Data.Char
import Data.Function (on)
import Data.List (elemIndex, groupBy, group, intercalate, maximumBy, sort)

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

-- Phone exercise

-- 1.
type Digit = Char

type Presses = Int

newtype DaPhone = DaPhone [Button]

data Button = Button
  { digit :: Digit,
    characters :: String
  }

phone :: DaPhone
phone =
  DaPhone
    [ Button '1' "1",
      Button '2' "abc2",
      Button '3' "def3",
      Button '4' "ghi4",
      Button '5' "jkl5",
      Button '6' "mno6",
      Button '7' "pqrs7",
      Button '8' "tuv8",
      Button '9' "wxyz9",
      Button '*' "^*",
      Button '0' "+ 0",
      Button '#' ".,#"
    ]

-- 2.

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

getMaybeIndex :: Maybe Int -> Int
getMaybeIndex Nothing = -1
getMaybeIndex (Just x) = x

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone@(DaPhone (x : xs)) c
  | isUpper c = ('*', 1) : reverseTaps phone (toLower c)
  | c `elem` characters x = [(digit x, getMaybeIndex (elemIndex c (characters x)) + 1)]
  | otherwise = reverseTaps (DaPhone xs) c

messageToTaps = concatMap $ reverseTaps phone

messagesToTaps :: DaPhone -> [String] -> [[(Digit, Presses)]]
messagesToTaps phone = map messageToTaps

-- 3

getPresses (digit, presses) = presses

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps =
  foldr
    (\(digit1, presses1) presses2 -> presses1 + presses2)
    0

fingerTaps2 :: [(Digit, Presses)] -> Presses
fingerTaps2 =
  foldr
    ((+) . snd)
    0

-- 4

-- using groupBy bc the question didn't specify
-- if cap letters are distinct from lower case, 
-- so made it ignore caps. Can make it not ignore caps by using group

mostPopularLetterWithFrequency :: String -> (Int, Char)
mostPopularLetterWithFrequency message = maximumBy (compare `on` fst) $
                                    map (\group@(x : xs) -> (length group, x)) $
                                      groupBy (\a b -> toUpper a == toUpper b) $ sort $ concat $ words message

mostPopularLetter :: String -> Char
mostPopularLetter message = snd $ mostPopularLetterWithFrequency message

coolestLtr :: [String] -> Char
coolestLtr messages = mostPopularLetter $ concat messages

mostPopularWordWithFrequency :: String -> (Int, String)
mostPopularWordWithFrequency message = maximumBy (compare `on` fst) $
                                    map (\wordGroup@(x:xs) -> (length wordGroup, x)) $ group $ sort $ words message

coolestWord :: [String] -> String
coolestWord messages = snd $ mostPopularWordWithFrequency $ unwords messages

-- Hutton's Razor

--1 

data Expr = Lit Integer | Add Expr Expr 

eval :: Expr -> Integer 
eval (Lit x) = x 
eval (Add x y) = eval x + eval y

--2 

printExpr :: Expr -> String 
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y