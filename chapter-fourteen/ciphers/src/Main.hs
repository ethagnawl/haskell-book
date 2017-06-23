module Main where

import Data.Char
import Test.QuickCheck

-- Vigenère cipher

-- It looks like there's an off-by-one bug in my caeser cipher, so I'm
-- borrowing @dwayne's
-- github.com/dwayne/haskell-programming/blob/9af6fc2a106640b8f4eb7504f29fcffb89dd071d/ch9/Cipher.hs
-- The following only works on lowercase strings without spaces. Sorry.

encode :: Char -> Int
encode ch = ord ch - ord 'a'

decode :: Int -> Char
decode n = chr (ord 'a' + n)

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f n ch = decode $ mod (f (encode ch) n) 26

rightShift :: Int -> Char -> Char
rightShift = shift (+)

leftShift :: Int -> Char -> Char
leftShift = shift (-)

e :: Int -> String -> String
e n = map (rightShift n)

d :: Int -> String -> String
d n = map (leftShift n)

crypt string keyword direction = concatMap func sks
  where base = ord 'a'
        offset = (\x -> (ord x) - base)
        func = (\(s, k) -> direction (offset k) [s])
        keywords = take (length string) $ cycle keyword
        sks = zip string keywords

encrypt string keyword = crypt string keyword e

decrypt string keyword = crypt string keyword d

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf1 genSafeChar

genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

prop_isoCaeser = forAll genPos $ \n ->
                  forAll genSafeString $ \word ->
                    (d n $ e n word) == word

prop_isoVigenere = forAll genSafeString $ \string ->
                    forAll genSafeString $ \keyword ->
                      ((flip decrypt) keyword $ encrypt string keyword) == string

runQC = do
  quickCheck prop_isoCaeser
  quickCheck prop_isoVigenere

main = do
  putStrLn "main"
