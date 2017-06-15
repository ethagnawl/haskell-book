module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- https://github.com/dwayne/haskell-programming/blob/master/ch14/addition/Addition.hs#L136-L147
myMult :: (Eq a, Num a) => a -> a -> a
myMult x y
  | sy == 0 || sy == 1                 = go x y 0
  | sy == (-1) && (sx == 0 || sx == 1) = go y x 0
  | otherwise                          = go ax ay 0
  where
    sx = signum x
    sy = signum y
    ax = abs x
    ay = abs y
    go x 0 xy = xy
    go x y xy = go x (y - 1) (x + xy)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing) ,(3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do

  describe "Addition" $ do
    it "1 + 1 is greater han 1" $ do
      (1 + 1) > 1 `shouldBe` True

    it "2 + 2 equals 4" $ do
      (2 + 2) `shouldBe` 4

    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

  describe "division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)

    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

  describe "myMult" $ do
    it "10 multiplied by 0 is 0" $ do
      myMult 10 0 `shouldBe` 0

    it "0 multiplied by 10 is 0" $ do
      myMult 0 10 `shouldBe` 0

    it "10 multiplied by 10 is 100" $ do
      myMult 10 10 `shouldBe` 100

    it "10 multiplied by 20 is 200" $ do
      myMult 10 20 `shouldBe` 200

    it "20 multiplied by 10 is 200" $ do
      myMult 20 10 `shouldBe` 200
