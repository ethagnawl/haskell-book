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
