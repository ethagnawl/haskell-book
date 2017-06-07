data Trivial = Trivial

instance Eq Trivial where
  (==) t1 t2 = True
  (/=) t1 t2 = False

result = Trivial == Trivial -- True

data DayOfWeek = Sun | Mon | Tue | Weds | Thu | Fri | Sat deriving Show
data Date = Date DayOfWeek Int deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==)  (Date weekday dayOfMonth)
        (Date weekday' dayOfMonth') = weekday == weekday' &&
                                      dayOfMonth == dayOfMonth'

Date Thu 10 == Date Thu 10 -- True

-- 1
module TisAnIntegerModule where

  data TisAnInteger = TisAn Integer

  instance Eq TisAnInteger where
    (==) (TisAn v) (TisAn v') = v == v'

-- 2
module TwoIntegersModule where

  data TwoInteger = Two Integer Integer

  instance Eq TwoInteger where
    (==) (Two a b) (Two a' b') =  a == a' &&
                                  b == b'
-- 3
module StringOrIntModule where
  data StringOrInt = TisAnInt Int | TisAString String

  instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt b) = a == b
    (==) (TisAString a) (TisAString b) = a == b
    (==) _ _ = False

-- 4
module PairModule where

  data Pair a = Pair a a deriving Show

  instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') =  a == a' &&
                                    b == b'

-- 5
module TupleModule where

data Tuple a b = Tuple a b deriving Show
x = Tuple 2 2
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') =  a == a' &&
                                    b == b'

-- 6
module WhichModule where

  data Which a = ThisOne a | ThatOne a deriving Show

  instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

-- 7
module EitherOrModule where

  data EitherOr a b = Hello a | Goodbye b deriving Show

  instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'
    (==) _ _ = False

-- based on their type signatures (Integral a => a -> a -> (a, a)) what do
-- divMod and quotRem do?
-- return both quotient and remainder (? maths ...)
-- e.g. divMod 20 10 (2, 0)
-- e.g. divMod 20 11 (1, 9)

-- monomorphic from polymorphic
-- Prelude> let numId = id :: Num a => a -> a
-- Prelude> let intId = numId :: Integer -> Integer
-- can't go backwards, though
-- Prelude> let altNumId = intId :: Num a => a -> a
-- > Types can be made more specific, but not more general or polymorphic.

-- datatype order specifies Ord behavior
-- data MyBool = MyFalse| MyTrue deriving (Eq, Show)

-- instance Ord MyBool where
-- compare MyFalse _ = GT
-- compare _ MyFalse = LT
-- compare _ _ = EQ

-- MyTrue > MyFalse -- False - would have been True if Ord had been derived

-- 1
-- True
max (length [1, 2, 3]) (length [8, 9, 10, 11, 12]) -- 5

-- 2
-- True
compare (3 * 4) (3 * 5) -- LT

-- 3
-- False
compare "Julie" True

-- 4
-- True
(5 + 3) > (3 + 6) -- False

-- 1 / c
-- 2 / b
-- 3 / a
-- 4 / c
-- 5 / a

-- 1
-- False - no instance for Show
data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
printPerson $ Person False

-- 2
-- False - no Mood constraint / Mood doesn't implement Eq
data Mood = Blah | Woot deriving (Eq, Ord, Show)
settleDown :: Mood -> Mood
settleDown x =  if x == Woot
                then Blah
                else x

-- 3
-- a What values are acceptable inputs to that function?
-- Moods
-- b What will happen if you try to run settleDown 9? Why?
-- excepton - no instance for Num Mood
-- c What will happen if you try to run Blah > Woot? Why?
-- no instance of Ord
-- d

-- 4
-- no - missing Object in s1 / no show instance for function
type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool" "wut"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Ord, Show)
data Yeah = Yeah Bool deriving (Eq, Ord, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Ord, Show)

-- 1
-- Rocks and Yeah require values (String and Bool, respectively)
phew = Papu (Rocks "chases") (Yeah True)

-- 2
-- yes
truth = Papu (Rocks "chomsky") (Yeah True)

-- 3
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

equalityForall truth truth


-- 4
-- no - no instance of Ord for Papu
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

comparePapus truth phew -- True after extending (?) Ord where necessary

-- 1
-- no?
i :: Num a => a
-- i :: a
i = 1


-- 2
-- no?
-- f :: Float
-- f :: Num a => a
f :: Fractional a => a
f = 1.00

-- 3
-- yes
-- f :: Float
f :: Fractional a => a
f = 1.0

-- 4
-- yes
-- f :: Float
f :: RealFrac a => a
f = 1.0

-- 5
-- yes
-- freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- 6
-- yes?
-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x

-- 7
-- no
myX = 1 :: Int
sigmund :: Int -> Int
-- sigmund :: a -> a
sigmund x = myX

-- 8
-- no
myX = 1 :: Int
sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
sigmund' x = myX

-- 9
-- yes
-- a) You’ll need to import sort from Data.List.
import Data.List (sort)
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10
-- yes
import Data.List (sort)
-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11
-- no
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk func a b = func a == b
chk (\x -> x * x) 2 4 -- True

-- 2
-- Hint: use some arithmetic operation to combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = (+) (fromIntegral i) (f a)
arith (\x -> x * x) 10 10 -- 110
