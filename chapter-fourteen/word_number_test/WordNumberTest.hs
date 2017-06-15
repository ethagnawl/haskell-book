module WordNumberTest where

import Test.Hspec
-- import WordNumber (digitToWord, digits, wordNumber)

-- numbers into words
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
                  1 -> "one"
                  2 -> "two"
                  3 -> "three"
                  4 -> "four"
                  5 -> "five"
                  6 -> "six"
                  7 -> "seven"
                  8 -> "eight"
                  9 -> "nine"
                  0 -> "zero"
                  otherwise -> error "out of bounds"

digits :: Int -> [Int]
digits n = case n >= 10 of
            True -> digits (fst $ divMod n 10) ++ [snd $ divMod n 10]
            False -> [n]

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n

main :: IO ()
main = hspec $ do

  describe "digitToWord" $ do

    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"

    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do

    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]

    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do

    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"

    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
