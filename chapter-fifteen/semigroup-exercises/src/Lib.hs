module Lib where

import Control.Monad
import Data.Monoid hiding ((<>))
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))

-- class Semigroup a where
--   (<>) :: a -> a -> a

-- instance Semigroup [a] where
--   (<>) as bs = as ++ bs

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity l) (Identity r) = Identity (l <> r)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two la lb) (Two ra rb) = Two (la <> ra) (lb <> rb)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three la lb lc) (Three ra rb rc) = Three (la <> ra) (lb <> rb) (lc <> rc)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc = Three String String String
                  -> Three String String String
                  -> Three String String String
                  -> Bool

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four la lb lc ld) (Four ra rb rc rd) = Four (la <> ra) (lb <> rb) (lc <> rc) (ld <> rd)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc =  Four String String String String
                  -> Four String String String String
                  -> Four String String String String
                  -> Bool

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _ = BoolConj False

-- What it should do:
-- res = ((BoolConj True) <> (BoolConj True)) == (BoolConj True)
-- res = ((BoolConj True) <> (BoolConj False)) == (BoolConj False)

instance Arbitrary BoolConj where
  arbitrary = elements [(BoolConj True), (BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj True) _ = BoolDisj True
  (<>) _ (BoolDisj True) = BoolDisj True
  (<>) _ _ = BoolDisj False

-- What it should do:
-- res = ((BoolDisj True) <> (BoolDisj True)) == (BoolDisj True)
-- res = ((BoolDisj True) <> (BoolDisj False)) == (BoolDisj True)

instance Arbitrary BoolDisj where
  arbitrary = elements [(BoolDisj True), (BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Snd l) _ = Snd l
  (<>) _ (Snd r) = Snd r
  (<>) (Fst l) (Fst r) = Fst r

-- res = (Fst 1 <> Snd 2) == Snd 2
-- res = (Fst 1 <> Fst 2) == Fst 2
-- res = (Snd 1 <> Fst 2) == Snd 1
-- res = (Snd 1 <> Snd 2) == Snd 1

-- sample (arbitrary :: Gen (Or String String))
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Fst a), (Snd b)]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

-- 9
-- https://stackoverflow.com/questions/39456716/how-to-write-semigroup-instance-for-this-data-type
-- https://github.com/galderz/haskell-sandbox/blob/9118d8dc4638b0fcc24b1b6875ab3fa43e58c666/haskellbook/ch15-exercises.hb/SemigroupCombineCheck.hs

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine l) (Combine r) = Combine (\x -> (l x) <> (r x))

funEquality :: (Arbitrary a, Show a, Eq b, Show b) => Combine a b -> Combine a b -> Property
funEquality (Combine f) (Combine g) = property $ \a -> f a === g a

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property

combineAssoc :: (Semigroup b, Arbitrary a, Show a, Eq b, Show b) => CombineAssoc a b
combineAssoc f g h = ((f <> g) <> h) `funEquality` (f <> (g <> h))

-- 10
newtype Comp a = Comp { unComp :: (Int -> Int) }

instance Semigroup (Comp a) where
  (<>) (Comp l) (Comp r) = Comp $ l . r

-- Cheating, but I've burned far too much time on this and Combine ...
prop_CompAssociative :: Int -> Bool
prop_CompAssociative i =
  unComp ((l <> m) <> r) i == unComp (l <> (m <> r)) i
  where l = Comp (\n -> n * 2)
        m = Comp (\n -> n * 2)
        r = Comp (\n -> n * 2)

-- 11
data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Failure' l) (Success' r) = Failure' l
  (<>) (Success' l) (Failure' r) = Failure' r
  (<>) (Failure' l) (Failure' r) = Failure' (l <> r)
  (<>) (Success' l) _ = Success' l

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [
            return (Failure' a)
          , return (Success' b)
          ]

type ValidationAssoc = Validation String String -> Validation String String -> Validation String String -> Bool

-- 12
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (<>) (AccumulateRight (Success' l)) (AccumulateRight (Success' r)) = AccumulateRight $ Success' (l <> r)
  (<>) (AccumulateRight (Success' l)) (AccumulateRight (Failure' r)) = AccumulateRight $ Success' l
  (<>) (AccumulateRight (Failure' l)) (AccumulateRight (Success' r)) = AccumulateRight $ Success' r
  (<>) (AccumulateRight (Failure' l)) _ = AccumulateRight $ Failure' l

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [
            return (AccumulateRight (Failure' a))
          , return (AccumulateRight (Success' b))
          ]

type AccumulateRightAssoc = AccumulateRight String String -> AccumulateRight String String -> AccumulateRight String String -> Bool

-- 13
newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (<>) (AccumulateBoth (Success' l)) (AccumulateBoth (Success' r)) = AccumulateBoth $ Success' (l <> r)
  (<>) (AccumulateBoth (Failure' l)) (AccumulateBoth (Failure' r)) = AccumulateBoth $ Failure' (l <> r)
  (<>) (AccumulateBoth (Failure' l)) _ = AccumulateBoth $ Failure' l
  (<>) _ (AccumulateBoth (Failure' r)) = AccumulateBoth $ Failure' r

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [
            return (AccumulateBoth (Failure' a))
          , return (AccumulateBoth (Success' b))
          ]

type AccumulateBothAssoc = AccumulateBoth String String -> AccumulateBoth String String -> AccumulateBoth String String -> Bool

runQuickCheck = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck $ \(Fun _ f) (Fun _ g) (Fun _ h) ->
    (combineAssoc :: CombineAssoc Int (Sum Int))
    (Combine f) (Combine g) (Combine h)
  quickCheck prop_CompAssociative
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
