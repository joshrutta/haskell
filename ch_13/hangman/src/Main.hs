module Main where

import Data.Char (toLower)
import Hangman (freshPuzzle, randomWord', runGame)

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
