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
