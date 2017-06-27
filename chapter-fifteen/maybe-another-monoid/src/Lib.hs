module Lib where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a' <- arbitrary
    elements [ (First' (Only a'))
             , (First' Nada)]

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only l)) _ = First' (Only l)
  mappend _ (First' (Only r)) = First' (Only r)
  mappend _ _ = First' Nada

-- What is the point of this function?
firstMappend :: First' a -> First' a -> First' a
firstMappend l r = mappend l r

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

runQuickcheck = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
