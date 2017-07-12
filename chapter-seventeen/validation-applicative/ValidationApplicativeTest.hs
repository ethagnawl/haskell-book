module ValidationApplicativeTest where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation' e a = Failure' e | Success' a deriving (Eq, Show)

instance Functor (Validation' e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation' e) where
  pure = Success'
  (<*>) (Success' f) (Success' v) = Success' (f v)
  (<*>) (Failure' f) (Success' _) = Failure' f
  (<*>) (Success' _) (Failure' f) = Failure' f
  (<*>) (Failure' l) (Failure' r) = Failure' (l `mappend` r)

-- sample' (arbitrary :: Gen (Validation' String Int))
instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation' e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure' e, Success' a]

instance (Eq e, Eq a) => EqProp (Validation' e a) where (=-=) = eq

main :: IO ()
main = do
  -- So fucking confused by what these tuples are doing ...
  -- Why can't I define instances of Validation' and pass those to functor
  -- and applicative like I did for ZipList and List?
  quickBatch $ functor (undefined :: Validation' String (Char, Int, Float))
  quickBatch $ applicative (undefined :: Validation' String (Char, Int, Float))
