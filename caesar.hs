module Cipher where

import Data.Char

shiftLetter :: Int -> Char -> Char
shiftLetter shift letter = chr ((ord letter + shift - 97) `mod` 26 + 97)

caesar :: String -> Int -> String
caesar xs key = map (shiftLetter key) xs

unCaesar :: [Char] -> Int -> [Char]
unCaesar xs key = map (shiftLetter (- key)) xs