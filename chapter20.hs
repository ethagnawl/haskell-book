data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z
  foldl _ z Nada = z
  foldl f z (Yep x) = f z x
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

-- Exercises: Library Functions

-- Implement the functions in terms of foldMap or foldr from Foldable,
-- then try them out with multiple types that have Foldable instances.

-- 1.
sum :: (Foldable t, Num a) => t a -> a
sum ta = getSum $ foldMap Sum ta

-- sum [Sum 22, Sum 44] -- Sum {getSum = 66}
-- sum [1,2,3,4,5] -- 15

-- 2.
product :: (Foldable t, Num a) => t a -> a
product ta = getProduct $ foldMap Product ta

-- print $ product [Product 22, Product 44] -- Product {getProduct = 968}
-- print $ product [1,2,3,4,5] -- 120

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem needle haystack = foldr (\x b -> b || x == needle) False haystack

-- elem 1 [1,2,3] -- True
-- elem 0 [1,2,3] -- False
-- elem "hi" ["goodbye", "hello", "hi"] -- True

-- 4.
-- Feeling pretty meh about this one. I feel like the foldable instance
-- could be better leveraged.
import Data.Maybe
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum ta = if (head == Nothing)
              then Nothing
              else Just $ foldr compare' (fromJust head) lst
  where lst = toList ta
        safeHead [] = Nothing
        safeHead (x:xs) = Just x
        head = safeHead lst
        compare' = (\x y -> case (compare x y) of
                              EQ -> x
                              LT -> x
                              GT -> y)

-- 5.
import Data.Maybe
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum ta = if (head == Nothing)
              then Nothing
              else Just $ foldr compare' (fromJust head) lst
  where lst = toList ta
        safeHead [] = Nothing
        safeHead (x:xs) = Just x
        head = safeHead lst
        compare' = (\x y -> case (compare x y) of
                              EQ -> x
                              LT -> y
                              GT -> x)

-- 6.
-- We were supposed to used fold/foldMap, but whatevs ...
null' :: (Foldable t) => t a -> Bool
null' ta = case (length ta == 0) of
              True -> True
              False -> False

-- 7.
length' :: (Foldable t) => t a -> Int
length' ta = foldr (\_ n -> n + 1) 0 ta

-- length' [] -- 0
-- length' [1,2,3] -- 3
-- length' (Left 0) -- 0
-- length' (Right 0) -- 1

-- 8.
toList :: (Foldable t) => t a -> [a]
toList ta = foldr (\a b -> [a] `mappend` b) [] ta

-- toList [1,2,3] -- [1,2,3]
-- toList (Left 0) -- []
-- toList (Right 0) -- [0]

-- 9.
fold :: (Foldable t, Monoid m) => t m -> m
fold tm = foldMap id tm

-- fold [[1,2,3], [4]] -- [1..4]
-- fold [(Sum 5), (Sum 25)] -- Sum {getSum = 30}

-- 10.
-- Unlike fold, foldMap has a function as its first argument. Unlike foldr,
-- the first (function) argument of foldMap must explicitly map each element of
-- the structure to a Monoid.

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f ta = foldr (\a b -> b `mappend` (f a)) mempty ta

-- foldMap' Sum [1,2,3,4] -- Sum {getSum = 10}
-- foldMap' Product [1,2,3,4] -- Product {getProduct = 24}

-- Chapter Exercises

-- 1.
-- https://github.com/lukleh/haskell-book-exercises/blob/gh-pages/ch20/ch20_20.6_1.hs

data Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap _ _ = mempty


-- 2.

data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

-- 3.

data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

-- 4.

data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b `mappend` f b'

-- 5.

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b `mappend` f b' `mappend` f b''

-- Punting on this one.
-- I don't think the following counts because the f never becomes t. I can get the
-- t a -> f a version to compile, but it throws instance errors whenever I try
-- to use it.
filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF predicate xs = foldMap
                        (\x ->
                          case (predicate x) of
                            True -> (pure x)
                            False -> mempty)
                        xs

-- <interactive>:95:1:
--     No instance for (Show (f0 [Char])) arising from a use of ‘print’
--     The type variable ‘f0’ is ambiguous
--     Note: there are several potential instances:
--       instance (Show a, Show b) => Show (Either a b)
--         -- Defined in ‘Data.Either’
--       instance forall (k :: BOX) (f :: k -> *) (a :: k).
--                Show (f a) =>
--                Show (Alt f a)
--         -- Defined in ‘Data.Monoid’
--       instance Show a => Show (Dual a) -- Defined in ‘Data.Monoid’
--       ...plus 23 others
--     In a stmt of an interactive GHCi command: print it
