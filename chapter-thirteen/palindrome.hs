import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

foundPalindrome :: IO ()
foundPalindrome =
  putStrLn "It's a palindrome!" >>
  exitSuccess

didNotFindPalindrome :: IO ()
didNotFindPalindrome =
    putStrLn "Not a palindrome! Try again!" >>
    return ()

isPalindromeWord word = word == reverse word

isPalindromeSentence sentence =
  isPalindromeWord $
  filter (\x -> elem x ['a'..'z']) $
  map toLower sentence

palindrome :: IO ()
palindrome = forever $ do
  possiblePalindrome <- getLine
  case (isPalindromeWord possiblePalindrome) of
    True -> foundPalindrome
    False -> didNotFindPalindrome

palindromeSentences :: IO ()
palindromeSentences = forever $ do
  possiblePalindromeSentence <- getLine
  case (isPalindromeSentence possiblePalindromeSentence) of
    True -> foundPalindrome
    False -> didNotFindPalindrome
