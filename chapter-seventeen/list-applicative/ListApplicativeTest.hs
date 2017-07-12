-- I'm losing my intuition for the subject matter and increasingly feeling
-- like I'm lost and flailing about.

module ListApplicativeTest where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend a Nil = a
  mappend Nil a = a
  mappend (Cons l ls) rs = Cons l $ ls `mappend` rs

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (Cons f fs) <*> vs = (fmap f vs) `mappend` (fs <*> vs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    car <- arbitrary
    cdr <- arbitrary
    return (Cons car (Cons cdr Nil))

instance Eq a => EqProp (List a) where (=-=) = eq

main :: IO ()
main = do
  -- Very confused by these tuples and what purpose they serve. Shouldn't
  -- we be using the Arbitrary instance to generate these values?
  let magic = (Cons ('w', 't', 'f') Nil)
  quickBatch $ monoid magic
  quickBatch $ functor magic
  quickBatch $ applicative magic
