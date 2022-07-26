{-# LANGUAGE BlockArguments #-}
module CipherWithInput where

import Data.Char (toLower)
import Cipher (caesar, unCaesar)
import ChapterExercises (vigenere, unVigenere)

cipherWithInput = do 
    putStrLn "Please enter valid word to encode"
    word <- getLine 
    case word of
         [c] -> encode word  
         _   -> putStrLn "Your key must\
                     \ be a single character" 

encode :: String -> IO ()
encode word = do
    putStrLn "Please enter a valid cipher to use (caesar, vigenere): "
    cipher <- getLine
    case map toLower cipher of 
        "caesar"   -> caesar' word >>= putStrLn
        "vigenere" ->  vigenere' word >>= putStrLn
        _          -> putStrLn "Please enter either caesar or vigenere"

vigenere' :: String -> String
vigenere' word = do
    putStrLn "Please enter a valid word key: " 
    key <- getLine 
    case key of 
        [c] -> vigenere key word  
        _   -> "Your key must be a single character" 

caesar' :: String -> String
caesar' word = do
    putStrLn "Please enter a valid word key: " 
    key <- getLine 
    case key of 
        [c] -> caesar word (read key :: Int)  
        _   -> "Your key must be a single character"