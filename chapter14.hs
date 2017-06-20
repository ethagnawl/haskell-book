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

-- 4. multiplication
multAssociative x y z = x * (y * z) == (x * y) * z
quickCheck multAssociative -- +++ OK, passed 100 tests.

multCommutative x y = x * y == y * x
quickCheck multCommutative -- +++ OK, passed 100 tests.

-- 5.
-- We mentioned in one of the first chapters that there are some
-- laws involving the relationship of quot and rem and div and mod.
-- Write QuickCheck tests to prove them.
prop_quotRem' _ 0 = True
prop_quotRem' x y = (quot x y) * y + (rem x y) == x
quickCheck prop_quotRem' -- +++ OK, passed 100 tests.

prop_divMod _ 0 = True
prop_divMod x y = (div x y) * y + (mod x y) == x
quickCheck prop_divMod -- +++ OK, passed 100 tests.

-- 6.
prop_powerAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)
quickCheck prop_powerAssociative -- Fails with various combinations of 0s
                                 -- and negative numbers.

prop_powerCommutative x y = x ^ y == y ^ x

quickCheck prop_powerCommutative -- Fails for various combinations of 0s
                                 -- and (at least) 1 2.

-- 7.
prop_reverseTwice lst = (reverse $ reverse lst) == (id lst)
quickCheck prop_reverseTwice -- +++ OK, passed 100 tests.

-- 8.
-- This could probably be better tested using dynamic function generation.
prop_Dollar' x y = ((+) x $ y * y) == (x + (y * y)) -- +++ OK, passed 100 tests.

-- 9.
-- See if these two functions are equal:
-- foldr (:) == (++)
-- foldr (++) [] == concat

prop_foldrCons x y = foldr (:) [] x == (++) [] x
quickCheck prop_foldrCons -- +++ OK, passed 100 tests.

prop_foldrConcat :: [[Int]] -> Bool
prop_foldrConcat x = foldr (++) [] x == concat x
quickCheck prop_foldrConcat -- +++ OK, passed 100 tests.

-- 10.
someF n xs = length (take n xs) == n
prop_someF = someF
quickCheck prop_someF
-- *** Failed! Falsifiable (after 3 tests and 2 shrinks): 1 []

-- 11.
-- Finally, this is a fun one. You may remember we had you com-
-- pose read and show one time to complete a “round trip.” Well,
-- now you can test that it works:
prop_readShow x = (read (show x)) == x
quickCheck prop_readShow -- +++ OK, passed 100 tests.
