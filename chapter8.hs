-- Recursion is self-referential composition.

-- intermission
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

applyTimes 5 (+1) 5
-- 4 + 6
-- 3 + 7
-- 2 + 8
-- 1 + 9
-- 10

-- In logic, ⊥ corresponds to false

-- A partial function is one which does not handle all of its inputs. A
-- total function is one that does

-- 1/d
-- 2/b
-- 3/d
-- 4/b

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appendCatty :: String -> String
appendCatty = cattyConny "whoops"

frappe :: String -> String
frappe = flippy "haha"

-- 1
appendCatty "woohoo" -- whoops mrow woohoo

-- 2
frappe "1" -- 1 mrow haha

-- 3
frappe (appendCatty "2") -- whoops mrow 2 mrow haha

-- 4
appendCatty (frappe "blue") -- whoops mrow blue mrow haha

-- 5
cattyConny (frappe "pink") (cattyConny "green" (appendCatty "blue"))
-- pink mrow haha mrow whoops mrow green whoops mrow blue

-- 6
cattyConny (flippy "Pugs" "are") "awesome" -- are mrow pugs mrow awesome

-- 1
dividedBy 15 2
15 - 2
-- 2, 13 (subtracted 1 times)
-- 2, 11 (subtracted 2 times)
-- 2, 9 (subtracted 3 times)
-- 2, 7 (subtracted 4 times)
-- 2, 5 (subtracted 5 times)
-- 2, 3 (subtracted 6 times)
-- 2, 1 (subtracted 7 times)

-- 2
sumFrom1to 1 = 1
sumFrom1to n = n + sumFrom1to (n - 1)
sumFrom1to 10 -- 55

-- 3
mult :: Integral a => a -> a -> a
mult _ 0 = 0
mult x y = x + mult x (y - 1)

mult 10 3 -- 30

data DividedResult = Result Integer | DividedByZero deriving Show

-- https://github.com/nackjicholson/haskellbook-solutions/blob/master/chapter8/exercises.hs
dividedBy 0 _ = Result (0, 0)
dividedBy _ 0 = DividedByZero
dividedBy num denom = go absNum absDenom 0
  where multiplier = signum num * signum denom
        absNum = abs num
        absDenom = abs denom
        go n d count
          | n < d = Result (count * multiplier, n)
          | otherwise = go (n - d) d (count + 1)

-- The McCarthy 91 function yields x − 10 when x > 100 and 91 otherwise.
mc91 n = case n > 100 of
          True -> n - 10
          False -> mc91 . mc91 $ n + 11

mc91 111 -- 101
mc91 50 -- 91

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
                  otherwise -> error "out of bounds"

digits :: Int -> [Int]
digits n = case n >= 10 of
            True -> digits (fst $ divMod n 10) ++ [snd $ divMod n 10]
            False -> [n]

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n

-- wordNumber 12324546 == "one-two-three-two-four-five-four-six" -- True
