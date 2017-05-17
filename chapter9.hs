eftBool :: Bool -> Bool -> [Bool]
eftBool bool = (::) $ not bool

eftA :: (Enum a, Ord a) => a -> a -> [a]
eftA begin end
  | begin > end = []
  | otherwise = begin : eftA (succ begin) end

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftA

eftInt :: Int -> Int -> [Int]
eftInt = eftA

eftChar :: Char -> Char -> [Char]
eftChar = eftA

-- 1
module MyWordsModule where

myWords :: String -> [String]
myWords [] = []
myWords (' ':words) = myWords words
myWords words = (:) (takeWhile (/= ' ') words)
                    (myWords $ dropWhile (/= ' ') words)

-- 2
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines ('\n':words) = myLines words
myLines words = (:) (takeWhile (/= '\n') words)
                    (myLines $ dropWhile (/= '\n') words)

shouldEqual = ["Tyger Tyger, burning bright",
               "In the forests of the night",
               "What immortal hand or eye",
               "Could frame thy fearful symmetry?" ]

myLines sentences == shouldEqual -- True

-- extract common functionality from myWords and myLines and parameterize
-- split character
myThing :: Char -> String -> [String]
myThing splitAt str =
    if str == [] then []
    else if (head str == splitAt) then myThing splitAt $ tail str
    else (:)  (takeWhile (/= splitAt) str)
              (myThing splitAt $ dropWhile (/= splitAt) str)

myWords' words = myThing ' ' words
myWords' "foo bar baz"

myLines' lines = myThing '\n' lines
myLines' sentences

mySqr = [x^2 | x <- [1..5]]

-- 4, 16
one = [x | x <- mySqr, rem x 2 == 0]

-- [] -- no squares > 50
two = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

-- [] -- still no squares > 50
three = take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

-- return vowels
myString xs = [x | x <- xs, elem x "aeiou"]
myString "a sad lad is a bad dad"

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1
myTuples = [(x, y) | x <- mySqr, y <- myCube]

-- 2
mySmallTuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3
mySmallTuplesLength = length mySmallTuples -- 15


blah = enumFromTo 'a' 'z'
-- :sprint blah -- _ => totally unevaluated

take 1 blah
-- :sprint blah -- 'a':_ => partially unevaluated

take 2 blah
-- :sprint blah -- 'a' : 'b' :_ => partially unevaluated

-- Normal Form => expression fully evaluated 2 * 2 = 4
-- Weak head normal form (WHNF) is a larger set and contains both the possibility
-- that the expression is fully evaluated (normal form) and the possibility that
-- the expression has been evaluated to the point of arriving at a data
-- constructor or lambda awaiting an argument.

tup = (1, 2) -- WHNF & NF => anything in normal form is also in WHNF
tup2 = (1, 2 + 2) -- WHNF => + hasn't been evaluated yet
lam = \x -> x * 10 -- WHNF & NF => can't be reduced further until lamda is applied

-- Bottom Madness
-- 1 / blow up
-- 2 / value
-- 3 / blow up
-- 4 / value
-- 5 / blow up
-- 6 / value
-- 7 / blow up
-- 8 / value
-- 9 / value
-- 10 / blow up

-- Is it normal form?
-- 1 / NF
-- 2 / WHNF
-- 3 / WHNF
-- 4 / WHNF
-- 5 / neither
-- 6 / WHNF
-- 7 / NF

-- A common mantra for performance sensitive code in Haskell is, “lazy in the
-- spine, strict in the leaves.”

-- More Bottoms
-- 1 / blow up
-- 2 / value
-- 3 / blow up

-- 4
-- maps chars in xs to bools based on vowel/consonant
itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

-- 5
-- a / raise each of 1..10 to the power of 2
-- b / [1, 10, 20]
-- c / [15, 15, 15]

-- 6
import Data.Bool (bool)
result2 = map (\x -> bool x (-x) (x == 3)) [1..10] -- [1,2,-3,4,5,6,7,8,9,10]

-- Filtering
-- 1
res = filter (\n -> (mod n 3) == 0) [1..30] -- [3,6,9,12,15,18,21,24,27,30]

-- 2
cnt = length . filter (\n -> (mod n 3) == 0) -- 10

-- 3
myFilter str = filter (\word -> word /= "the" && word /= "a" && word /= "an") $ words str
myFilter "the brown dog was a goof"
