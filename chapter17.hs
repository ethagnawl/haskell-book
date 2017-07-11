-- (Just (* 2)) <*> (Just 2) => (Just 4)
-- (pure (* 2)) <*> (Just 2) => (Just 4)
-- liftA (* 2) (Just 2)      => (Just 4)

-- f a b c = a + b + c
-- liftA3 f (Just 1) (Just 1) (Just 1) => Just 3

-- (fmap (++) (Just "hi")) <*> (Just "bye") => Just "hibye"

-- fmap length $ (++) <$> getLine <*> getLine => 6

-- With Applicative, we have a Monoid for our structure and
-- function application for our values!

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

-- Pretty sure this isn't what the authors had in mind, but this is the only
-- solution I can come up with that actually sums the two Integers.
-- Relatedly, this is the only solution that I've seen _anywhere_ which actually
-- sums the numbers. Now, that's not to say that it's the most correct or
-- anything, but I think that points to a problem with this question: If
-- actually summing the numbers wasn't the point, then `sum` should not have
-- been used. If it was, this question is either too difficult or the chapter
-- needed to better seed the solution. I'm also going to push back against the
-- authors' suggestion to "ignore" difficult questions. I'm not very motivated
-- to tackle the next question/section after having pounded my head against my
-- keyboard for multiple hours and achieved nothing. Giving up makes me
-- feel like I must have missed something important and that I'm not ready to
-- move on. I told them this on Twitter (and got no response) but I would gladly
-- pay for a comprehensive list of answers to check my work against and work
-- backwards from if/when I do get stuck.
-- </barf>

import Data.Maybe
import Data.Tuple

summed :: Maybe Integer
summed =  (pure sum) <*> (fmap fst tuple) <*> (fmap snd tuple)
  where x' = Sum $ fromMaybe 0 x
        y' = Sum $ fromMaybe 0 y
        tuple = Just (x', y')

-- Exercise: Identity Instance

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity v) = Identity (f v)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity v) = Identity (f v)

-- Exercise: Constant Instance

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant l) (Constant r) = Constant (mappend l r)

