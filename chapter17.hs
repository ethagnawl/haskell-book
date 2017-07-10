-- (Just (* 2)) <*> (Just 2) => (Just 4)
-- (pure (* 2)) <*> (Just 2) => (Just 4)
-- liftA (* 2) (Just 2)      => (Just 4)

-- f a b c = a + b + c
-- liftA3 f (Just 1) (Just 1) (Just 1) => Just 3

-- (fmap (++) (Just "hi")) <*> (Just "bye") => Just "hibye"

-- fmap length $ (++) <$> getLine <*> getLine => 6

-- Exercises: Lookups

-- 1.

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6]) -- => Just 9

-- 2.

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z -- Just (6, 5)

-- 3.

import Data.List (elemIndex)

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = fmap max x <*> y -- => Just 3

-- 4.

xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

-- Wherein I raise my white flag:
summed :: Maybe Integer
summed = Just $ (sum [(getX x), (getY y)])
  where
    getX (Just x') = x'
    getX (Nothing) = 0
    getY (Just y') = y'
    getY (Nothing) = 0

