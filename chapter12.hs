-- left of Either is used for whatever case is going to cause the work to stop.

-- A lifted type, which includes any datatype you could define yourself, is any that can be inhabited by bottom.

-- kind * is the kind of all standard lifted types, while types that have the kind # are unlifted.

-- Chapter Exercises

-- 1
-- id :: a -> a
-- :k a -- *

-- 2
-- r :: a -> f a
-- What are the kinds of a and f?
-- a -- *
-- f -- * -> *

-- 1
replaceTheWithA' "" = []
replaceTheWithA' string = firstWord' : replaceTheWithA' restWords
  where words' = words string
        firstWord = head words'
        firstWord' = case (firstWord == "the") of
                      True -> ["a"]
                      False -> [firstWord]
        restWords = unwords $ tail words'

replaceTheWithA :: String -> String
replaceTheWithA string = unwords $ concat $ replaceTheWithA' string

-- 2
-- Write a recursive function that takes a text/string, breaks it into
-- words, and counts the number of instances of ”the” followed by
-- a vowel-initial word.
isVowel l = case l of
              'a' -> True
              'e' -> True
              'i' -> True
              'o' -> True
              'u' -> True
              'y' -> True
              _ -> False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel str = count + countTheBeforeVowel rest
  where isThe w = w == "the"
        words' = words str
        firstTwoWords = take 2 words'
        count = if length firstTwoWords /= 2
                  then 0
                  else
                    if  isThe $ head firstTwoWords &&
                        isVowel (head $ head $ tail firstTwoWords)
                      then 1
                      else 0
        rest = unwords $ tail words'

-- 3
-- Return the number of letters that are vowels in a word.
-- Hint: it’s helpful to break this into steps. Add any helper func-
-- tions necessary to achieve your objectives.
-- a) Test for vowelhood
-- b) Return the vowels of a string
-- c) Count the number of elements returned
countVowels :: String -> Integer
countVowels str = length $
                  filter (True &&) $
                  map isVowel $
                  foldr (++) "" $
                  words str

-- Validate the word
countSyllableParts :: (Bool -> Bool) -> String -> Int
countSyllableParts filterFunc str = length $
                                    filter filterFunc $
                                    map isVowel $
                                    foldr (++) "" $
                                    words str

countVowels' = countSyllableParts ((==) True)
countConsonants = countSyllableParts ((==) False)

newtype Word' = Word' String deriving (Eq, Show)
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = result
  where
    vowels = countVowels' str
    consonants = countConsonants str
    result = if vowels > consonants then Nothing else Just $ Word' str

-- It's only natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = (+) 1 (natToInteger nat)

integerToNat :: Integer -> Maybe Nat
integerToNat int =  if int < 0
                      then Nothing
                      else Just (reduceNatToZero int)
  where reduceNatToZero 0 = Zero
        reduceNatToZero n = Succ (reduceNatToZero (n - 1))

