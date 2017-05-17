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
