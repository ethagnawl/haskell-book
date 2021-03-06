-- folds => catamorphisms (morphing between categories?)

-- foldl => fold leftward, not starting at the left - doh.
--          this might have been the cause of some of my issues with
--          point-free notation in the roll-your-own-stdlib functions
--          in chapter 9

-- All folds recurse over the spine in the same direction; the difference between
-- left folds and right folds is in the association, or parenthesization, of the
-- folding function and, thus, which direction the folding or reduction proceeds.

-- 1 / b

-- 2
foldl (flip (*)) 1 [1..3]
foldl (flip (*)) (* 1 1 ) [2..3]
foldl (flip (*)) (* 2 (* 1 1 ) [3]
foldl (flip (*)) (* 3 (* 2 (* 1 1 ) []

-- 3 / c

-- 4 / a (?)

-- 5
-- a) foldr (++) ["woot", "WOOT", "woot"]
foldr (++) "" ["woot", "WOOT", "woot"]

-- b) foldr max [] "fear is the little death"
foldr max "" (words "fear is the little death")

-- c) foldr and True [False, True]
foldr (&&) True [False, True]

-- d) This one is more subtle than the previous. Can it ever return a different answer?
-- foldr (||) True [False, True]
-- No.

-- e) foldl ((++) . show) "" [1..5]
foldr ((++) . show) "" [1..5]

-- f) foldr const 'a' [1..5]
import Data.Char
foldr const 'a' $ map intToDigit [1..5]

-- g) foldr const 0 "tacos"
foldl const "" "tacos"

-- h) foldl (flip const) 0 "burritos"
foldl const "0" "burritos"

-- i) foldl (flip const) 'z' [1..5]
foldr (flip const) 'z' [1..5]

-- foldl is generally inappropriate with lists that are or could be infinite,
-- but the combination of the forced spine evaluation with non-strictness means
-- that it is also usually inappro- priate even for long lists, as the forced
-- evaluation of the spine affects performance negatively. Because foldl must
-- evaluate its whole spine before it starts evaluating values in each cell, it
-- accumulates a pile of unevaluated values as it traverses the spine.

-- In most cases, when you need a left fold, you should use foldl'.  This
-- function, called “fold-l-prime,” works the same except it is strict.  In
-- other words, it forces evaluation of the values inside cons cells as it
-- traverses the spine, rather than accumulating unevaluated expres- sions for
-- each element of the list. The strict evaluation here means it has less
-- negative effect on performance over long lists.

-- Exercises: Database Processing

-- module DatabaseThings where

import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
              , DbNumber 9001
              , DbString "Hello, world!"
              , DbDate (UTCTime
                        (fromGregorian 1921 5 1)
                        (secondsToDiffTime 34123)) ]

-- 1. Write a function that filters for DbDate values and returns a list of the UTCTime values inside them.
-- https://github.com/dwayne/haskell-programming/blob/master/ch10/Database.hs
-- dates db = [t | (DbDate t) <- db]
-- that's cheating, tho.

isDbDate (DbDate _) = True
isDbDate _ = False
getDate (DbDate date) = date
getDate _ = error "not supported"
filterDbDate db = map getDate $ filter isDbDate db
-- filterDbDate theDatabase = [DbDate 1911-05-01 09:28:43 UTC,DbDate 1921-05-01 09:28:43 UTC]

-- 2
-- Write a function that filters for DbNumber values and returns a list of the
-- Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = [number | (DbNumber number) <- db]

-- 3
-- Write a function that gets the most recent date.
-- mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = maximum $ filterDbDate db

-- 4
-- Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 $ filterDbNumber db

-- 5
-- Write a function that gets the average of the DbNumber values.
-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb db = dbNumbersSum / dbNumbersLength
  where dbNumbers = filterDbNumber db
        dbNumbersLength = (fromIntegral $ length dbNumbers) :: Double
        dbNumbersSum = (fromIntegral $ sumDb db) :: Double

-- scans exercises
fibs = 1 : scanl (+) 1 fibs

-- 1
firstNFibs n = take n fibs
first20Fibs = firstNFibs 20

-- 2
fibsLessThan100 = takeWhile (< 100) fibs

-- 3
factorial n = factorial'
  where index = (n - 1)
        factorial' = scanl (*) 1 [2..n] !! index


-- chapter exercises

-- warm-up
-- 1
stops = "pbtdkg"
vowels = "aeiou"

-- a) Write a function that takes inputs from stops and vowels and makes
-- 3-tuples of all possible stop-vowel-stop com- binations. These will not all
-- correspond to real words in English, although the stop-vowel-stop pattern
-- is common enough that many of them will.
threeTupleCombos :: [a] -> [a] -> [(a, a, a)]
threeTupleCombos xs ys = [(x, y, x') | x <- xs, y <- ys, x' <- xs]

stopVowelStopCombosBeginningWithP = filter filterFunc combos
  where filterFunc = (\(s, _, _) -> s == 'p')
        combos = threeTupleCombos stops vowels

nouns = ["dog", "turkey", "Istanbul", "wallet"]
verbs = ["open", "close", "run", "frolic"]
nounVerbNounCombos nouns verbs = threeTupleCombos nouns verbs

-- 2 What does the following mystery function do? What is its type?  Try to get
-- a good sense of what it does before you test it in the REPL to verify it.
avgWordLength :: String -> Int
avgWordLength str = div (sum (map length (words str))) (length (words str))

-- 3
-- We’d really like the answer to be more precise. Can you rewrite that using
-- fractional division?
preciseAvgWordLength :: String -> Double
preciseAvgWordLength str = dividend / divisor
  where dividend = fromIntegral (sum (map length (words str))) :: Double
        divisor = fromIntegral (length (words str)) :: Double

-- Rewriting functions using folds

-- 1. myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. myAny returns True if a -> Bool applied to any of the values in the list
-- returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny func = foldr (\x xs -> xs || func x) False

-- 3. Write two versions of myElem. One version should use folding and the
-- other should use any.
myElemFold :: Eq a => a -> [a] -> Bool
myElemFold needle = foldr (\x xs -> xs || x == needle) False

myElemAny:: Eq a => a -> [a] -> Bool
myElemAny needle = any (== needle)

-- 4. Implement myReverse, don’t worry about trying to make it lazy.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
-- myReverse "blah" -- "halb"
-- myReverse [1..5] -- [5,4,3,2,1]

-- 5. Write myMap in terms of foldr. It should have the same behavior as the
-- built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap func = foldr (\x xs -> func x : xs) []

-- 6. Write myFilter in terms of foldr. It should have the same behavior as
-- the built-in filter.
myFilter :: (a -> Bool)
myFilter func = foldr (\x xs -> if func x then x : xs else xs) []

-- 7. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8. squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap func lst = foldr (++) [] $ map func lst

-- 9. squishAgain flattens a list of lists into a list. This time re-use the
-- squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = concat . map id

-- 10. myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value that the
-- comparison returned GT for.
-- https://github.com/dwayne/haskell-programming/pull/1
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy func xs = foldr compare' seed xs
  where seed = (last xs)
        compare' l r =  case (func l r) of
                          GT -> l
                          _ -> r

-- 11. myMinimumBy takes a comparison function and a list and returns the
-- least element of the list based on the last value that the comparison
-- returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy func xs = foldr compare' seed xs
  where seed = (last xs)
        compare' l r =  case (func l r) of
                          GT -> r
                          _ -> l
