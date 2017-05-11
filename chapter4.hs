data Mood = Wut | Weet deriving Show

-- 1
Mood

-- 2
-- Wut | Weet

-- 3
-- signature should be: changeMood :: Mood -> Mood
-- type instead of value

-- 4
changeMood :: Mood -> Mood
changeMood Wut = Weet
changeMood Weet = Wut

changeMood Weet -- Wut
changeMood Wut -- Weet

-- all Num instances
-- Int => ranged (legacy)
-- Integer => arbitrarily small/large

-- Float => single precision
-- Double => double precision
-- Rational => ration of two integers 1/2 precise, but not as efficient as
--             scientific
-- Scientific => space eficient and almost arbitrarily precise



-- 1
False
-- no value `true`
not True && True

-- 2
False
-- not (x = 6) => type mismatch

-- 3
True
(1 * 2) > 5

-- 4
False
-- no data constructors for Merry and Happy
["Merry"] > ["Happy"]

-- 5
False
-- type mismatch
(++) ["1", "2", "3"] ["look at me!"] -- closest approximation

-- type alias
type Name = String

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "Simpsons"]
allAwesome = [awesome, alsoAwesome]

-- 1
length :: [a] -> Int

-- 2
-- a
5

-- b
3

--c
2

-- d
5

-- 3
6 / length [1,2,3] -- fails because the result of length is Int, not Fractional

-- 4
6 `div` length [1,2,3] -- will work

-- 5
Bool
True

-- 6
Bool
False

-- 7
-- a
True == (length allAwesome == 2) -- True

-- b
-- False - list members must be of same type

-- c
5 == (length allAwesome + length awesome)

-- d
result = False == ((8 == 8) && ('b' < 'a'))

-- e
-- False - 9 isn't a Bool
-- (8 == 8) && 9

--8
isPalindrome lst = lst == (reverse lst)
isPalindrome [1, 2, 1] -- True
isPalindrome [2, 1] -- False

-- 9
abs' n = if n < 0 then n - n - n else n
abs' (-22)
abs' 22

-- 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))


-- 1
x = (+)
f xs = (+) 1 $ toInteger $ length xs
f "foo" -- 4

-- 2
(\x -> x) 22

-- 3
(\(x:xs) -> x) [1, 2, 3]

-- 4
(\(a, _) -> a) (22, 42)


-- 1
-- c

-- 2
-- b

-- 3
-- a

-- 4
-- d

