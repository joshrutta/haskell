module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"

digits :: Int -> [Int]
digits n = go n []
  where
    go n dig_list
      | n >= 10 = go (n `div` 10) ((n `mod` 10) : dig_list)
      | otherwise = n : dig_list

wordNumber :: Int -> String
wordNumber n = intercalate "-" $ map digitToWord $ digits n