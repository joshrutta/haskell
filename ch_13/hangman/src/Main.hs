module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

numGuesses = 7

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
      in  l >= minWordLength
      &&  l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

stringToNothing :: String -> [Maybe Char]
stringToNothing = map (const Nothing)

freshPuzzle :: String -> Puzzle
freshPuzzle puzzleWord = Puzzle puzzleWord (stringToNothing puzzleWord) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wordToBeGuessed _ _) charGuess = charGuess `elem` wordToBeGuessed

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessedChars) charGuess = charGuess `elem` guessedChars

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar x =
  case x of
    (Just letter) -> letter
    Nothing       -> '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s) where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
      else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle _ _ guessed) guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  putStrLn $ "You have " ++ show (1 + numGuesses - length guessed) ++ " guesses left"
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
                \ character, pick\
                \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
                \ word, filling the word\
                \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
                \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  when (length guessed > numGuesses) $ do
    putStrLn "You lose!"
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _) =
  when (all isJust filledInSoFar) $ do
    putStrLn $ "You win! The word is " ++ word
    exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do 
  gameOver puzzle 
  gameWin puzzle 
  putStrLn $ 
    "Current puzzle is: " ++ show puzzle 
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of 
    [c] -> handleGuess puzzle c >>= runGame 
    _   -> putStrLn "Your guess must\
                     \ be a single character" 

main :: IO ()
main = do
  word <- randomWord' 
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

