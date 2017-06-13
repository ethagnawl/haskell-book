module Cipher where

import Data.Char

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
