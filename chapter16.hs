-- fmap (+ 1) (Just 1) == liftM1 (+ 1) (Just 1)

-- f n = fmap (10/) (4, 5) == (4, 2.5)

-- Exercises: Be Kind
-- Given a type signature, determine the kinds of each type variable:

-- 1. What’s the kind of a? in a -> a
-- *

-- 2. What are the kinds of b and T? (The T is capitalized on purpose!)
-- in a -> b a -> T (b a)
-- b: * -> *
-- T: * -> *

-- 3. What’s the kind of c? in c a b -> c b a
-- * -> * -> *

-- Functor is a typeclass for function application “over”, or “through”,
-- or “past” some structure f that we want to ignore and leave untouched.

-- Think of anything that isn’t the final type argument of our f in Functor as
-- being part of the structure that the functions being lifted should be
-- oblivious to.

-- (fmap . fmap [Just 5]) == fmap (fmap (+1)) [Just 5]

-- Exercises: Heavy Lifting
-- 1.
a = fmap (+1) $ read "[1]" :: [Int]

-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c = fmap (*2) $ (\x -> x - 2)
c 1 == -2

-- 4.
d = fmap ((return '1' ++) . show) $ (\x -> [x, 1..3])
res = d 0 == "1[0,1,2,3]"

-- 5.
e :: IO Integer
e = fmap (*3) changed
  where ioi = readIO "1" :: IO Integer
        changed = fmap read $
                  fmap ("123" ++) $
                  fmap show ioi

