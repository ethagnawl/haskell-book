-- Functor -> Applicative -> Monad

-- Whenever you’ve implemented an instance of Monad for a type
-- you necessarily have an Applicative and a Functor as well.

-- Functor and Applicative can be derived in terms of Monad
-- fmap: [1..3] >>= return . (+1)
-- fmap (more verbosely): [1..3] >>= (\n -> return (1 + n))

-- return == pure

-- Monad, in a sense, is a generalization of concat! The unique part of
-- Monad is the following function:
-- import Control.Monad (join)
-- join :: Monad m => m (m a) -> m a
-- compare with:
-- concat :: [[a]] -> [a]

-- The answer is the exercise
-- Write bind in terms of fmap and join.
-- Fear is the mind-killer, friend. You can do it.

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m
-- bind (\n -> [n, 1]) [1..5] -- [1,1,2,1,3,1,4,1,5,1]

-- twiceWhenEven :: [Integer] -> [Integer]
-- twiceWhenEven xs = do
--   x <- xs
--   if even x
--     then [x*x, x*x]
--     else [x*x]

-- twiceWhenEven [1,2,3] => [1,4,4,9]

-- Short Exercise: Either Monad
-- Implement the Either Monad.

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (<*>) (Second l) (Second r) = Second (l r)
  (<*>) (First l) (Second r) = First l
  (<*>) (Second l) (First r) = First r

instance Monad (Sum a) where
  return = pure
  (>>=) (First m) _ = First m
  (>>=) (Second m) f = f m

-- (First 5) >>= (\n -> return (n * n)) -- (First 5)
-- (Second 5) >>= (\n -> return (n * n)) -- (Second 25)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(First a), (Second b)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

quickBatch $ functor (undefined :: Sum String (Int, Double, Char))
quickBatch $ applicative (undefined :: Sum String (Int, Double, Char))
quickBatch $ monad (undefined :: Sum String (Int, Double, Char))

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

res' = (CountMe 1 (* 2)) <*> (CountMe 1 2)          -- CountMe 2 4
res = (CountMe 100 100) >>= (\n -> CountMe 100 100) -- CountMe 200 100

mcomp'' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp'' f g a = g a >>= f

-- what are the _m_s here?
mcomp'' (\n -> return $ n * n) (\n -> return $ n * n * n) 3 -- 729
mcomp'' (\n -> return $ n ++ "!") (\n -> return $ n ++ "?") "hello" -- hello?!

-- Kleisli composition: function composition with monadic structure hanging off
-- the functions we’re composing.
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

import Control.Monad ((>=>))

-- return seems redundant?
drop1 :: Monad m => [a] -> m [a]
drop1 lst = return $ drop 1 lst

(>=>) drop1 drop1 [1..5] -- [3,4,5]

-- Chapter Exercises

-- Write Monad instances for the following types. Use the QuickCheck
-- properties we showed you to validate your instances.

-- 1.

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad Nope where
  (>>=) NopeDotJpg _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

quickBatch $ functor (undefined :: Nope (Int, Double, Char))
quickBatch $ applicative (undefined :: Nope (Int, Double, Char))
quickBatch $ monad (undefined :: Nope (Int, Double, Char))

-- 2.

data PhhhbbtttEither b a = Left a | Right b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right b) = Right b
  fmap f (Left a) = Left (f a)

instance Applicative (PhhhbbtttEither b) where
  pure = Left
  (<*>) (Left l) (Left r) = Left (l r)
  (<*>) (Left _) (Right r) = Right r
  (<*>) (Right l) _ = Right l

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Left m) f = f m
  (>>=) (Right m) _ = Right m

-- sample' (arbitrary :: Gen (PhhhbbtttEither Int Int))
instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    elements [(Left a), (Right b)]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

quickBatch $ functor (undefined :: PhhhbbtttEither String String (Int, Double, Char))
quickBatch $ applicative (undefined :: PhhhbbtttEither String (Int, Double, Char))
quickBatch $ monad (undefined :: PhhhbbtttEither String (Int, Double, Char))

-- 3.

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity v) = Identity (f v)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

-- sample' (arbitrary :: Gen (Identity Int))
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

quickBatch $ functor (undefined :: Identity (Int, Double, Char))
quickBatch $ applicative (undefined :: Identity (Int, Double, Char))
quickBatch $ monad (undefined :: Identity (Int, Double, Char))

-- 4.

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend a Nil = a
  mappend Nil a = a
  mappend (Cons l ls) rs = Cons l $ ls <> rs

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (Cons f fs) <*> vs = (fmap f vs) <> (fs <*> vs)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons head' tail') f = (f head') <> (tail' >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    one <- arbitrary
    two <- arbitrary
    three <- arbitrary
    -- Lazy. Consider trying one of the solutions laid out here:
    -- https://byorgey.wordpress.com/2016/09/20/the-generic-random-library-part-1-simple-generic-arbitrary-instances/
    elements [
               Nil
             , (Cons one Nil)
             , (Cons one (Cons two Nil))
             , (Cons one (Cons two (Cons three Nil)))
             ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

quickBatch $ functor (undefined :: List (Int, Double, Char))
quickBatch $ applicative (undefined :: List (Int, Double, Char))
quickBatch $ monad (undefined :: List (Int, Double, Char))

-- Write the following functions using the methods provided by
-- Monad and Functor. Using stuff like identity and composition is fine,
-- but it has to typecheck with types provided.

-- 1.

import Control.Monad

j :: Monad m => m (m a) -> m a
j m = join m -- Is this not cheating?

-- 2.

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM -- Still not cheating?

-- 3.

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2 -- ...

-- 4.

a :: Monad m => m a -> m (a -> b) -> m b
a = (flip (<*>))

-- 5.

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh as f = mapM f as

-- 6.

-- Illegal type signature: ‘(Monad m) => [m a] -> m [a] flipType’ ???
flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id
