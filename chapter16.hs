-- fmap (+ 1) (Just 1) == liftM1 (+ 1) (Just 1)

-- f n = fmap (10/) (4, 5) == (4, 2.5)

-- Exercises: Be Kind
-- Given a type signature, determine the kinds of each type variable:

-- 1. What’s the kind of a? in a -> a
-- *

-- 2. What are the kinds of b and T? (The T is capitalized on purpose!)
-- in a -> b a -> T (b a)
-- b: * -> *
-- T: * -> *

-- 3. What’s the kind of c? in c a b -> c b a
-- * -> * -> *

-- Functor is a typeclass for function application “over”, or “through”,
-- or “past” some structure f that we want to ignore and leave untouched.

-- Think of anything that isn’t the final type argument of our f in Functor as
-- being part of the structure that the functions being lifted should be
-- oblivious to.

-- (fmap . fmap [Just 5]) == fmap (fmap (+1)) [Just 5]

-- Exercises: Heavy Lifting
-- 1.
a = fmap (+1) $ read "[1]" :: [Int]

-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c = fmap (*2) $ (\x -> x - 2)
c 1 == -2

-- 4.
d = fmap ((return '1' ++) . show) $ (\x -> [x, 1..3])
res = d 0 == "1[0,1,2,3]"

-- 5.
e :: IO Integer
e = fmap (*3) changed
  where ioi = readIO "1" :: IO Integer
        changed = fmap read $
                  fmap ("123" ++) $
                  fmap show ioi


-- Exercises: Instances of Func
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

-- quickCheck $ \x -> functorIdentity (x :: [Int])

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- li x = functorCompose (+1) (*2) (x :: [Int])
-- quickCheck li

-- 1.
newtype Identity' a = Identity' a deriving (Show, Eq)

instance Functor Identity' where
  fmap f (Identity' a) = Identity' (f a)

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return (Identity' a)

prop_IdentityFunctorIdentity x = functorIdentity (x :: (Identity' String))
prop_IdentityFunctorCompose x = functorCompose ("foo" ++) ("bar" ++) (x :: (Identity' Int))

quickCheck prop_IdentityFunctorIdentity
quickCheck prop_IdentityFunctorCompose

-- 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a _) = Pair a' a'
    where a' = f a

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

prop_PairFunctorIdentity x = functorIdentity (x :: (Pair String))
prop_PairFunctorCompose x = functorCompose ("foo" ++) ("bar" ++) (x :: (Pair String))

quickCheck prop_PairFunctorIdentity
quickCheck prop_PairFunctorCompose

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

prop_TwoFunctorIdentity x = functorIdentity (x :: (Two Int Int))
prop_TwoFunctorCompose x = functorCompose ("foo" ++) ("bar" ++) (x :: (Two String String))

quickCheck prop_TwoFunctorIdentity
quickCheck prop_TwoFunctorCompose

-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

prop_ThreeFunctorIdentity x = functorIdentity (x :: (Three Int Int Int))
prop_ThreeFunctorCompose x = functorCompose ("foo" ++) ("bar" ++) (x :: (Three String String String))

quickCheck prop_ThreeFunctorIdentity
quickCheck prop_ThreeFunctorCompose

-- 5

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

prop_Three'FunctorIdentity x = functorIdentity (x :: (Three' Int Int Int))
prop_Three'FunctorCompose x = functorCompose ("foo" ++) ("bar" ++) (x :: (Three' String String String))

quickCheck prop_Three'FunctorIdentity
quickCheck prop_Three'FunctorCompose

-- 6.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

prop_FourFunctorIdentity x = functorIdentity (x :: (Four Int Int Int Int))
prop_FourFunctorCompose x = functorCompose ("foo" ++) ("bar" ++) (x :: (Four String String String String))

quickCheck prop_FourFunctorIdentity
quickCheck prop_FourFunctorCompose

-- 7.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return (Four' a a' a'' b)

prop_Four'FunctorIdentity x = functorIdentity (x :: (Four' Int String))
prop_Four'FunctorCompose x = functorCompose ("foo" ++) ("bar" ++) (x :: (Four' Int String))

quickCheck prop_Four'FunctorIdentity
quickCheck prop_Four'FunctorCompose

-- 8
-- Not as is because GHC complains that:
-- • Expected kind ‘* -> *’, but ‘Trivial’ has kind ‘*’
-- So, you'd need to add an inhabitant(?), like so:
data Trivial a = Trivial a deriving (Eq, Show)

instance Functor Trivial where
  fmap f (Trivial a) = Trivial (f a)

-- Exercise: Possibly
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- eta contract to drop the obvious argument(s)

-- Short Exercise

-- 1
-- Write a Functor instance for a datatype identical to Either. We’ll
-- use our own datatype because Either also already has a Functor
-- instance.

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

-- hint: applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b

-- 2. Why is a Functor instance that applies the function only to First,
-- Either’s Left, impossible? We covered this earlier.

-- The kind will be incorrect because there will be a trailing * -> *.

-- Chater Exercises

-- 1.
data Bool = False | True deriving (Eq, Show)
-- No. Bool's kind is *, instead of  * -> *

-- 2.
-- Either
data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

-- 3.
-- Maybe
data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

-- 4. Use the kinds to guide you on this one, don’t get too hung up
-- on the details.
newtype Mu f = InF { outF :: f (Mu f) }
-- Yes. Mu :: (* -> *) -> * applied to one arg (a la Either) becomes * -> *

-- 5. Again, just follow the kinds and ignore the unfamiliar parts
import GHC.Arr
data D = D (Array Word Word) Int Int
-- No. D :: *

-- Rearrange the arguments to the type constructor of the datatype
-- so the Functor instance works.

-- 1.

data Sum b a = First a | Second b deriving (Eq, Show)

-- instance Functor (Sum e) where -- Why did the book use `e` here?
instance Functor (Sum b) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

-- 2

data Company a c b = DeepBlue a c | Something b deriving (Eq, Show)

instance Functor (Company a c) where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.

-- 1.

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant b) where
  fmap _ Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K a) = K a

-- 3

-- {-# LANGUAGE FlexibleInstances #-}

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K a b = K a deriving (Eq, Show)

data Pair a b = Pair Int String deriving (Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

-- 4.

data EvilGoateeConst a b = GoatyConst b deriving (Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- 5.
-- Do you need something extra to make the instance work?

data LiftItOut f a = LiftItOut (f a) deriving (Show)
-- LiftItOut [1, 2, 3]
-- fmap (+ 2) (LiftItOut [1, 2, 3]) => LiftItOut [3, 4, 5]

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut a) = LiftItOut $ fmap f a

-- 6.
data Parappa f g a = DaWrappa (f a) (g a) deriving (Show)

-- d = DaWrappa [1] [2]
-- fmap (+ 2) d => DaWrappa [3] [4]

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa l r) = DaWrappa (fmap f l) (fmap f r)

-- 7.

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething l r) = IgnoringSomething l $ (fmap f r)

-- 8.

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Show)

-- g needs to be a functor because f gets fmapped over it.
instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious l m r) = Notorious l m (fmap f r)

-- 9.

data List a = Nil | Cons a (List a) deriving (Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10.

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Show)

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats l m r) = MoreGoats (fmap f l) (fmap f m) (fmap f r)

-- 11.

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read f') = Read (fmap f f')
