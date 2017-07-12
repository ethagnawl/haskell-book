module ZipListApplicativeTest where

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

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons h t) = Cons h (take' (n - 1) t)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  (<*>) (ZipList' flst) (ZipList' vlst) = ZipList' (flst <*> vlst)

-- sample' (arbitrary :: Gen (ZipList' (List Int)))
instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

main :: IO ()
main = do
  let magic = (ZipList' (Cons ("I still don't"
                              , "understand WTF this tuple"
                              , "is doing...") Nil))

  -- Why didn't book request Monoid instance? Is there no value in it or is it
  -- not possible? Looks like there's no Monoid instance for
  -- Control.Applicative's ZipList, so maybe not?
  -- quickBatch $ monoid magic

  quickBatch $ functor magic
  quickBatch $ applicative magic
