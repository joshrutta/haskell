module Cipher where

import Data.Char

-- 65 is ord 'A', 97 is ord 'a'

shiftLetter :: Int -> Char -> Char
shiftLetter shift letter
  | isUpper letter = chr ((ord letter + shift - 65) `mod` 26 + 65)
  | isLower letter = chr ((ord letter + shift - 97) `mod` 26 + 97)
  | otherwise = letter

caesar :: String -> Int -> String
caesar xs key = map (shiftLetter key) xs

unCaesar :: [Char] -> Int -> [Char]
unCaesar xs key = map (shiftLetter (- key)) xs