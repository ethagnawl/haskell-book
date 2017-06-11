module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse, sort)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = puzzle ++ guesses
    where puzzle = (intersperse ' ' $ fmap renderPuzzleChar discovered)
          guesses = "\nGuessed so far: " ++ (sort guessed)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter validWord aw)
  where validWord w =
          let l = length (w :: String)
          in  l > minWordLength &&
              l < maxWordLength &&
              all (flip (elem) ['a'..'z']) w

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (min, max)
  return $ wl !! randomIndex
  where min = 0
        max = (length wl) - 1

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle wordToGuess = Puzzle wordToGuess discovered guessed
  where discovered = map (const Nothing) wordToGuess
        guessed = []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wordToGuess _ _) guess = elem guess wordToGuess

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) guess = elem guess guesses

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

fillInWordCharacter :: Puzzle -> Char -> Puzzle
fillInWordCharacter (Puzzle wordToGuess filledInSoFar guesses) c =
  Puzzle wordToGuess newFilledInSoFar guesses
  where zipper guessed wordChar guessChar = if wordChar == guessed
                                              then Just wordChar
                                              else guessChar
        newFilledInSoFar = zipWith (zipper c) wordToGuess filledInSoFar

fillInGuessCharacter :: Puzzle -> Char -> Puzzle
fillInGuessCharacter (Puzzle wordToGuess filledInSoFar s) c =
  Puzzle wordToGuess filledInSoFar (c : s)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInWordCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInGuessCharacter puzzle guess)

maxGuesses :: String -> Int
maxGuesses wordToGuess = (length wordToGuess) `div` 2

guessesRemaining :: Puzzle -> Int
guessesRemaining (Puzzle wordToGuess _ guesses) =
  (maxGuesses wordToGuess) - (length guesses) + 1

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > (maxGuesses wordToGuess) then
    do  putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
        exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _) =
  if all isJust filledInSoFar then
    do  putStrLn message
        exitSuccess
  else return ()
  where message = "You corectly guessed, '" ++ wordToGuess ++ "'! You win!"

validGuess c = elem c ['a' .. 'z']

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn $ "Guesses remaining: " ++ show (guessesRemaining puzzle)
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] ->
      if validGuess c
        then handleGuess puzzle c >>= runGame
        else putStrLn "Guesses must be lowercase alphanumeric characters."
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
