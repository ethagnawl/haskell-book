-- Chapter Exercises

-- Validating "numbers into words"
-- See cat chapter-fourteen/word_number_test/WordNumberTest.hs

-- Using QuickCheck

-- 1. -- for a function
half :: Fractional a => a -> a
half x = x / 2

-- this property should hold
halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity n = (n == halfIdentity n) == True

quickCheck prop_halfIdentity -- +++ OK, passed 100 tests.

-- 2.
import Data.List (sort)

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listOrdered lst = (listOrdered $ sort lst) == True
prop_listOrdered' = listOrdered . sort

-- 3. Now we’ll test the associative and commutative properties of addition:
plusAssociative x y z = x + (y + z) == (x + y) + z
quickCheck plusAssociative -- +++ OK, passed 100 tests.

plusCommutative x y = x + y == y + x
quickCheck plusCommutative -- +++ OK, passed 100 tests.
