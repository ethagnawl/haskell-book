module Lib where

import Control.Monad
import Data.Monoid hiding ((<>))
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _= Trivial

instance Monoid Trivial where
  mappend = (<>)
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

monoidLeftIdentity x = mempty <> x == x -- left identity
monoidRightIdentity x = x <> mempty == x -- right identity

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity l) (Identity r) = Identity (l <> r)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two la lb) (Two ra rb) = Two (la <> ra) (lb <> rb)

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty $ mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

-- What it should do:
-- res = (BoolConj True) `mappend` mempty == (BoolConj True)
-- res = (mempty `mappend` (BoolConj False)) == (BoolConj False)

instance Arbitrary BoolConj where
  arbitrary = elements [(BoolConj True), (BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 5
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj l) (BoolDisj r) = BoolDisj (l || r)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

-- What it should do:
-- res = ((BoolDisj True) `mappend` mempty) == (BoolDisj True)
-- res = (mempty `mappend` (BoolDisj False)) == (BoolDisj False)

instance Arbitrary BoolDisj where
  arbitrary = elements [(BoolDisj True), (BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 6

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine l) (Combine r) = Combine (\x -> (l x) <> (r x))

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ mempty
  mappend = (<>)

funEquality :: (Arbitrary a, Show a, Eq b, Show b) => Combine a b -> Combine a b -> Property
funEquality (Combine f) (Combine g) = property $ \a -> f a === g a

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property

combineAssoc :: (Semigroup b, Monoid b, Arbitrary a, Show a, Eq b, Show b) => CombineAssoc a b
combineAssoc f g h = ((f <> g) <> h) `funEquality` (f <> (g <> h))

-- Cheating a bit, but ... :/
defaultCombine = Combine (\n -> Sum $ n + n)
prop_CombineMonoidLeftIdentity :: Int -> Bool
prop_CombineMonoidLeftIdentity n = mempty <> (unCombine defaultCombine $ n) == (unCombine defaultCombine $ n)
prop_CombineMonoidRightIdentity :: Int -> Bool
prop_CombineMonoidRightIdentity n = (unCombine defaultCombine $ n) <> mempty == (unCombine defaultCombine $ n)

-- 7
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (<>) (Comp l) (Comp r) = Comp $ l . r

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

prop_CompAssociative :: String -> Bool
prop_CompAssociative i =
  unComp ((l <> m) <> r) i == unComp (l <> (m <> r)) i
  where l = Comp (\n -> n ++ n)
        m = Comp (\n -> n ++ n)
        r = Comp (\n -> n ++ n)

prop_CompMonoidLeftIdentity :: String -> Bool
prop_CompMonoidLeftIdentity i =
  mempty <> (unComp c $ i) == (unComp c $ i)
  where c = Comp (\n -> n ++ n)

prop_CompMonoidRightIdentity :: String -> Bool
prop_CompMonoidRightIdentity i =
  (unComp c $ i) <> mempty == (unComp c $ i)
  where c = Comp (\n -> n ++ n)

-- 8
-- TODO:
-- newtype's using record syntax are still a mystery to me ...

-- newtype Mem s a = Mem { runMem :: s -> (a,s) }

-- instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
--   mempty = Mem $ \s -> (mempty, s)
--   mappend (Mem {runMem = f}) (Mem {runMem = g}) =
--     Mem $ \x -> let (a, b) = g x
--                     (c, d) = f b
--                 in (a <> c, d)

-- f' = Mem $ \s -> ("hi", s + 1)

-- print $ runMem (f' <> mempty) 0 == ("hi",1)
-- print $ runMem (mempty <> f') 0 == ("hi",1)
-- print $ (runMem mempty 0 :: (String, Int)) == ("",0)
-- print $ runMem (f' <> mempty) 0 == runMem f' 0 == True
-- print $ runMem (mempty <> f') 0 == runMem f' 0 == True

runQuickCheck = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck $ \(Fun _ f) (Fun _ g) (Fun _ h) ->
    (combineAssoc :: CombineAssoc Int (Sum Int))
    (Combine f) (Combine g) (Combine h)
  quickCheck prop_CombineMonoidLeftIdentity
  quickCheck prop_CombineMonoidRightIdentity
  quickCheck prop_CompAssociative
  quickCheck prop_CompMonoidLeftIdentity
  quickCheck prop_CompMonoidRightIdentity
