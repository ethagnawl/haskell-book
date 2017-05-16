addOne 10 = 10 + 1
addOne 11 = 11 + 1

-- 1 - all of them
mTh x y z = x * y * z
mTh' x y = \z -> x * y * z
mTh'' x = \y -> \z -> x * y * z
mTh''' = \x -> \y -> \z -> x * y * z

mTh 1 2 3 -- 6
mTh' 1 2 3 -- 6
mTh'' 1 2 3 -- 6
mTh''' 1 2 3 -- 6

-- 2
(mth 3) :: Num a => a -> a -> a

-- 3
-- a
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = (\n -> n + 1)

-- b
addFive = (\x -> (\y -> (if x > y then y else x) + 5))

-- c
mflip f x y = f y x

-- Patterns are matched against values, or data constructors, not types.


-- 1
-- a) What is the type of k?
k :: (a, b) -> a
k (x, y) = x

k1 = k ((4-1), 10)

-- b) What is the type of k2? Is it the same type as k1 or k3?
k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 = k (3, True)

-- c) Of k1, k2, k3, which will return the number 3 as the result?
-- k1 and k3

-- 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f    (a, b, c)    (d, e, f) =  ((a, d), (c, f))

-- 1
-- functionC x y = if (x > y) then x else y
functionC x y =
  case (x > y) of
    True -> x
    False -> y

-- 2
-- ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

-- 3
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

-- 1
dodgy 1 0 -- 0

-- 2
dodgy 1 1 -- 11

-- 3
dodgy 2 2 -- 22

-- 4
dodgy 1 2 -- 21

-- 5
dodgy 2 1 -- 12

-- 6
oneIsOne 1 -- 11

-- 7
oneIsOne 2 -- 21

-- 8
oneIsTwo 1 -- 21

-- 9
oneIsTwo 2 -- 22

-- 10
oneIsOne 3 -- 31

-- 11
oneIsTwo 3 -- 23

-- 1
avgGrade x
  | otherwise = 'Z'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

-- always 'Z'

-- 2
avgGrade x
  | y >= 0.59 = 'D'
  | y >= 0.8 = 'B'
  | y < 0.59 = 'F'
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  where y = x / 100

-- avgGrade 90 -- 'D'
-- >= is eager and will match 90 to 'D'

-- 3/b
pal xs
  | xs == reverse xs = True
  | otherwise = False

-- 4 / anything which is an instance of Eq

-- 5
-- pal :: Eq xs => [xs] -> Bool

-- 6/c

-- 7
-- anything which is an instance of Num and Ord

-- 8
-- numbers :: (Num a, Num a', Ord a) => a -> a'

-- 1/d

-- 2/

f :: Char -> String
f x = [x]

g :: String -> [String]
g x = [x]

-- (g . f) :: Char -> [String]

-- 3/b

-- 4/b

-- 5/a
f x = x
-- (f True) :: Bool

-- 1
-- a
tensDigit x =
  snd $ divMod10 $ fst (divMod10 x)
  where divMod10 = (flip divMod) 10

-- b
-- yes

-- c
hundredsDigit x =
  snd $ divMod10 $ fst $ divMod10 $ fst (divMod10 x)
  where divMod10 = (flip divMod) 10

-- 2
foldBool :: a -> a -> Bool -> a
foldBool x y True = x
foldBool x y False = y

foldBool' x y bool
  | bool == True = x
  | otherwise = y

foldBool'' x y bool =
  case bool of
    True -> x
    False -> y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f t = f $ fst t

-- 4
module Arith4 where
  -- id :: a -> a
  -- id x = x
  roundTrip :: (Show a, Read a) => a -> a
  roundTrip = read . show

  main = do
    print (roundTrip 4)
    print (id 4)

-- 5
roundTrip = read . show

-- 6
module Arith4 where
  roundTrip :: (Show a, Read b) => a -> b
  roundTrip = read . show
  main = do
    print (roundTrip 222 :: Integer)
