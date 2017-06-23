module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List
import Data.List (intersperse, sort)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Test.QuickCheck

type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] [Char] deriving (Eq)

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

handleGuess :: Puzzle -> Char -> (String, Puzzle)
handleGuess puzzle guess = do
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      ("You already guessed that character, pick something else!",
       puzzle)
    (True, _) -> do
      ("This character was in the word, filling in the word accordingly",
       (fillInWordCharacter puzzle guess))
    (False, _) -> do
      ("This character wasn't in the word, try again.",
       (fillInGuessCharacter puzzle guess))

handleGuessIO :: Puzzle -> Char -> IO Puzzle
handleGuessIO puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  putStrLn msg
  return puzzle'
  where (msg, puzzle') = handleGuess puzzle guess

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
  where message = "You correctly guessed, '" ++ wordToGuess ++ "'! You win!"

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
        then handleGuessIO puzzle c >>= runGame
        else putStrLn "Guesses must be lowercase alphanumeric characters."
    _ -> putStrLn "Your guess must be a single character"

prop_fillInWordCharacter :: String -> Bool
prop_fillInWordCharacter word = expected == actual
  where word' = if (length word > 0) then nub word else "foo"
        discoveries = map (const Nothing) [1 .. (length word')]
        guesses = []
        guess = head word'
        puzzle = (Puzzle word' discoveries guesses)
        actual = fillInWordCharacter puzzle guess
        discoveries' = ((Just guess) : (drop 1 discoveries))
        expected = (Puzzle word' discoveries' [])

prop_handleGuessAlreadyGuessed :: String -> Bool
prop_handleGuessAlreadyGuessed word = expected == actual
  where word' = if (length word > 0) then nub word else "foo"
        discoveries = [Just guess]
        guess = head word'
        guesses = [guess]
        puzzle = (Puzzle word' discoveries guesses)
        actual = handleGuess puzzle guess
        expected = ("You already guessed that character, pick something else!",
                    (Puzzle word' discoveries [guess]))

prop_handleGuessSuccessful :: String -> Bool
prop_handleGuessSuccessful word = expected == actual
  where word' = if (length word > 0) then nub word else "foo"
        guess = head word'
        discoveries = map (const Nothing) [1 .. (length word')]
        discoveries' = (Just guess) : (drop 1 discoveries)
        guesses = []
        guesses' = []
        puzzle = (Puzzle word' discoveries guesses)
        actual = handleGuess puzzle guess
        expected = ("This character was in the word, filling in the word accordingly",
                    (Puzzle word' discoveries' guesses'))

prop_handleGuessUnsuccessful :: String -> Bool
prop_handleGuessUnsuccessful word = expected == actual
  where word' = nub word
        word'' = if (length word' > 3) then word' else "foo"
        word''' = drop 1 word''
        guess = head $ take 1 word''
        discoveries = map (const Nothing) [1 .. (length word''')]
        guesses = []
        guesses' = [guess]
        puzzle = (Puzzle word''' discoveries guesses)
        actual = handleGuess puzzle guess
        expected = ("This character wasn't in the word, try again.",
                    (Puzzle word''' discoveries guesses'))

runQC = do
  quickCheck prop_fillInWordCharacter
  -- I cheated (maybe?) and moved the IO portion of handleGuess into a
  -- wrapper function (handleGuessIO), so I didn't have to muck with testing the IO
  -- action itself. Though, I believe this is possible using niceties provided by
  -- QuickCheck and/or parameter-izing the IO action and stubbing something
  -- introspectable in during testing.  It'd also be nice to construct generators
  -- which return _good_ values, instead of interrogating the value provided to
  -- prop_handleGuess* and swapping in a default if the value is bogus.
  quickCheck prop_handleGuessAlreadyGuessed
  quickCheck prop_handleGuessSuccessful
  quickCheck prop_handleGuessUnsuccessful

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
