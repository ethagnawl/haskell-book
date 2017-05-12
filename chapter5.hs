-- -- -> type constructor for functions
-- -- takes arguments and has no data constructors

-- -- The compiler gives the least specific and most general type it can.

-- -- fifteenDouble (specific) + fifteen (less specific) -- True
-- -- fifteenDouble (specific) + fifteenInt (specific) -- False (types no longer polymorphic)


-- -- multiple typeclass constraints
-- -- (Num a, Num b) => a -> b -> b
-- -- (Ord a, Num a) => a -> a -> Ordering

-- -- 1

-- -- a/c
-- -- not :: Bool -> Bool

-- -- b/d
-- -- length :: [a] -> Int

-- -- c/b
-- -- concat :: [[a]] -> [a]

-- -- d/a
-- -- head :: [a] -> a

-- -- e/e
-- -- (<) :: Ord a => a -> a -> Bool

-- -- sectioning - partially apply either argument of a binary operator
-- -- (^ 2)
-- -- (2 ^)

-- -- 1 / a
-- -- f :: a -> a -> a ->
-- -- x :: Char
-- -- f x :: Char -> Char -> Char

-- -- 2 / d
-- -- g :: a -> b -> c -> b
-- -- g 0 'c' "woot" :: Char

-- -- 3 / b
-- -- h :: (Num a, Num b) => a -> b -> b
-- -- h 1.0 2 :: Integer

-- -- 4 / c
-- -- h :: (Num a, Num b) => a -> b -> b
-- -- h 1 (5.5 :: Double) :: Double

-- -- 5 / a
-- -- jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
-- -- :t jackal "keyboard" "has the word jackal in it" :: [Char]
-- --

-- -- 6 / e
-- -- jackal :: (Ord a, Eq b) => a -> b -> a
-- -- jackal "keyboard" :: Eq b => b -> [Char]

-- -- 7 / d
-- -- kessel :: (Ord a, Num b) => a -> b -> a
-- -- (kessel 1 2) :: (Num a, Ord a) => a

-- -- 8 / a
-- -- kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
-- -- two = 2::Integer
-- -- :t (kessel 1 two)
-- -- (Num a, Ord a) => a

-- -- 9 / c
-- -- kessel :: (Ord a, Num b) => a -> b -> a
-- -- one = 1::Integer
-- -- :t kessel one 2 :: Integer

-- -- lowercase names in type signatures are necessarily polymorphic
-- -- uppercase names in type signatures are necessarily concrete

-- -- By default, type variables are resolved at
-- -- the left-most part of the type signature and are fixed once sufficient
-- -- information to bind them to a concrete type is available.

-- -- 1

-- -- 2
-- f :: a -> a -> a
-- f a a' = a

-- g :: a -> a -> a
-- g a a' = a'

-- -- 3
-- -- not sure i understand how there can be a fixed number of implementations here ...
-- f' :: a -> b -> b
-- f' a b = if a > 0 then "foo" else "bar"
-- f' 10 "wut"

-- -- 1
-- myConcat x = x ++ "!"
-- myConcat :: [Char] -> [Char]

-- -- 2
-- myMult x = (x / 3) * 5
-- myMult :: Fractional a => a -> a

--  -- 3
-- myTake x = take x "hey you"
-- myTake :: Integer -> [Char]

-- -- 4
-- myCom x = x > (length [1..10])
-- myCom :: (Ord a, Num a) => a -> Bool

-- -- 5
-- myAlph x = x < 'z'
-- myAlph :: Char -> Bool

-- -- 1/c

-- -- 2/a

-- -- 3/b

-- -- 4/c

-- 1
-- a
foo :: Integer
foo = (* 9) 6

--b
foo' :: (Integer, [Char])
foo' = head [(0,"doge"),(1,"kitteh")]

-- c
foo'' :: (Integer, [Char])
foo'' = head [(0 :: Integer ,"doge"),(1,"kitteh")]

-- d
foo''' :: Bool
foo''' = if False then True else False

-- e
foo'''' :: Int
foo'''' = length [1, 2, 3]

-- f
foo''''' :: Bool
foo''''' = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- 2
x = 5
y = x + 10
w = y * 10
w :: Integer

-- 3
x = 5
y = x + 5
z y = y * 10
z :: Num a => a -> a

-- 4
x = 5
y = x + 5
f = 4 / y
f :: Double -- ??

-- 5
x = "Julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z
f :: [Char]

-- 1
bigNum = (^) 5 $ 10
wahoo = bigNum -- ? $ 10

-- 2
x msg = print msg
y = print "woohoo!"
z = x "hello world"


-- 3
a = (+)
b = 5
c = a b 10
d = a c 200

-- 4
a = 12 + b
b = 10000 * c
c = 1


-- 2
f :: zed -> Zed -> Blah
-- [fully polymorphic, concrete, concrete]

-- 3
f :: Enum b => a -> b -> C
-- [fully polymorphic, constrained polymorphic, concrete]

-- 4
f :: f -> g -> C
-- [fully polymorphic, fully polymorphic, concrete]

-- 1
functionH :: [a] -> a
functionH (x:_) = x

-- 2
functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3
functionS :: (a, b) -> b
functionS (x, y) = y


-- 1
i :: a -> a
i a = a

-- 2
c :: a -> b -> a
c a b = a
c 2 False

-- 3
c'' :: b -> a -> b
c'' = c
c'' 2 False

-- 4
c' :: a -> b -> b
c' a b = b

-- 5
r :: [a] -> [a]
r (a:as) = as
r [] = []
r []

-- 6
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB = bToC . aToB
co (\a -> a * a) (\a -> a * a * a) 22

-- 7
a :: (a -> c) -> a -> a
a _ a = a
a id 10

-- 8
a' :: (a -> b) -> a -> b
a' f = f
a' (\a -> a * a) 22

-- 1
module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"

-- 2
module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x < y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"

-- 3
module Arith3Broken where

main :: IO ()
main = do
  print $ 1 + 2
  putStrLn $ show 10
  print (negate (-1))
  print ((+) 0 blah)
  where blah = negate 1

-- 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

-- 2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- 3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (,) (xz x) (yz y)

-- 4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ = fst $ yToWZ $ xToY

-- principal type is most generic possibility which still typechecks


