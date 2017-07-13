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

-- Exercise: Fixer Upper
-- Given the function and values provided, use (<$>) from Functor, (<*>) and
-- pure from the Applicative typeclass to fill in missing bits of the broken
-- code to make it work.

-- 1.
const <$> Just "Hello" <*> pure "World"

-- 2.
(,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- Exercises: List Applicative
-- See chapter-seventeen/list-applicative/ListApplicativeTest.hs

-- Exercises: ZipList Applicative
-- See chapter-seventeen/zip-list-applicative/ZipListApplicativeTest.hs

-- Exercise: Variations on Either
-- see chapter-seventeen/validation-applicative/ValidationApplicativeTest.hs

-- Chapter Exercises

-- Given a type that has an instance of Applicative, specialize the types
-- of the methods. Test your specialization in the REPL. One way to
-- do this is to bind aliases of the typeclass methods to “more concrete”
-- types that have the type we told you to fill in.

-- 1.

-- []
(pure :: (a -> [] a)) 4 -- [4]
((((<*>) :: [] (a -> b) -> [] a -> [] b) [(+1)]) [2]) -- [3]

-- 2.

-- IO
-- Methods
((pure :: a -> IO a)) 2 -- IO/print 2
((((<*>) :: IO (a -> b) -> IO a -> IO b) (pure (+2))) (pure 2)) -- 4

-- 3.

-- (,) a
-- https://github.com/lukleh/haskell-book-exercises/blob/gh-pages/ch17/ch17.adoc
((pure :: Monoid b => a -> ((,) b) a) "wut") :: (String, String) -- ("","wut")
((<*>) :: Monoid c => (,) c (a -> b) -> (,) c a -> (,) c b) ("WT", (++ "hurts")) ("F", "my brain ") -- ("WTF", 42)

-- 4.

-- Lifted from: https://github.com/lukleh/haskell-book-exercises/blob/gh-pages/ch17/ch17.adoc
-- No idea how to demonstrate this in the repl
(pure :: a -> ((-> e) a)
(<*>) :: ((->) e) (a -> b) -> ((->) e) a -> ((->) e) b

-- Write applicative instances for the following datatypes. Confused?
-- Write out what the type should be. Use the checkers library to validate
-- the instances.

-- 1.
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair l r) = Pair (f l) (f r)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair fl fr) (Pair vl vr) = Pair (fl vl) (fr vr)

-- sample (arbitrary :: Gen (Pair Int))
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

quickBatch $ functor (Pair ('a', 'b', 'c') ('d', 'e', 'f'))
quickBatch $ applicative (Pair ('a', 'b', 'c') ('d', 'e', 'f'))


-- 2.
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two l r) = Two l (f r)

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  (<*>) (Two fl fr) (Two vl vr) = Two (fl `mappend` vl) (fr vr)

-- sample' (arbitrary :: Gen (Two Int Int))
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

quickBatch $ functor (Two ('a', 'b', 'c') ('d', 'e', 'f'))
quickBatch $ applicative (Two ("a", "b", "c") ("d", "e", "f"))

-- 3.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three l m r) = Three l m (f r)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three fl fm fr) (Three vl vm vr) = Three (fl `mappend` vl) (fm `mappend` vm) (fr vr)

-- sample' (arbitrary :: Gen (Three Int Int Int))
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

quickBatch $ functor (Three ("w", "t", "f") ("f", "t", "w") (["w"], ["t"], ["f"]))
quickBatch $ applicative (Three ("w", "t", "f") ("f", "t", "w") (["w"], ["t"], ["f"]))

-- 4.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' l m r) = Three' l (f m) (f r)

instance (Monoid a) => Applicative (Three' a) where
  pure a = Three' mempty a a
  (<*>) (Three' fl fm fr) (Three' vl vm vr) = Three' (fl `mappend` vl) (fm vm) (fr vr)

-- sample' (arbitrary :: Gen (Three' Int Int))
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- Stealing test tuples from: https://github.com/lukleh/haskell-book-exercises/blob/8c96e2d36555221160f08b9c98107388bb9cdcbc/ch17/ch17_17.9_5.hs
-- Seriously, what the fuck is going on with these tuples? These are still magical to me.
quickBatch $ functor (Three' ("b", "w", [1]) ("b", "w", [1]) ("b", "w", [1]))
quickBatch $ applicative (Three' ("b", "w", [1]) ("b", "w", [1]) ("b", "w", [1]))

-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four l ml mr r) = Four l ml mr (f r)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (<*>) (Four fl fml fmr fr) (Four vl vml vmr vr) = Four (fl `mappend` vl) (fml `mappend` vml) (fmr `mappend` vmr) (fr vr)

-- sample' (arbitrary :: Gen (Four Int String Int String))
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

quickBatch $ functor (Four ("w", "t", "f") ("f", "t", "w") (["w"], ["t"], ["f"]) ("f", "u", "c"))
quickBatch $ applicative (Four ("w", "t", "f") ("f", "t", "w") (["w"], ["t"], ["f"]) ("f", "u", "c"))

-- 6.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' l ml mr r) = Four' l ml mr (f r)

instance (Monoid a) => Applicative (Four' a) where
  pure d = Four' mempty mempty mempty d
  (<*>) (Four' fl fml fmr fr) (Four' vl vml vmr vr) = Four' (fl `mappend` vl) (fml `mappend` vml) (fmr `mappend` vmr) (fr vr)

-- sample' (arbitrary :: Gen (Three Int Int Int))
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return (Four' a a' a'' b)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

quickBatch $ functor (Four' ("f", "u", "x") ("c", "k", "x") ("t", "h", "x") ("i", "s", "x"))
quickBatch $ applicative (Four' ("f", "u", "x") ("c", "k", "x") ("t", "h", "x") ("i", "s", "x"))
