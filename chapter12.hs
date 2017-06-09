-- left of Either is used for whatever case is going to cause the work to stop.

-- A lifted type, which includes any datatype you could define yourself, is any that can be inhabited by bottom.

-- kind * is the kind of all standard lifted types, while types that have the kind # are unlifted.

-- Chapter Exercises

-- 1
-- id :: a -> a
-- :k a -- *

-- 2
-- r :: a -> f a
-- What are the kinds of a and f?
-- a -- *
-- f -- * -> *

-- 1
replaceTheWithA' "" = []
replaceTheWithA' string = firstWord' : replaceTheWithA' restWords
  where words' = words string
        firstWord = head words'
        firstWord' = case (firstWord == "the") of
                      True -> ["a"]
                      False -> [firstWord]
        restWords = unwords $ tail words'

replaceTheWithA :: String -> String
replaceTheWithA string = unwords $ concat $ replaceTheWithA' string

-- 2
-- Write a recursive function that takes a text/string, breaks it into
-- words, and counts the number of instances of ”the” followed by
-- a vowel-initial word.
isVowel l = case l of
              'a' -> True
              'e' -> True
              'i' -> True
              'o' -> True
              'u' -> True
              'y' -> True
              _ -> False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel str = count + countTheBeforeVowel rest
  where isThe w = w == "the"
        words' = words str
        firstTwoWords = take 2 words'
        count = if length firstTwoWords /= 2
                  then 0
                  else
                    if  isThe $ head firstTwoWords &&
                        isVowel (head $ head $ tail firstTwoWords)
                      then 1
                      else 0
        rest = unwords $ tail words'

-- 3
-- Return the number of letters that are vowels in a word.
-- Hint: it’s helpful to break this into steps. Add any helper func-
-- tions necessary to achieve your objectives.
-- a) Test for vowelhood
-- b) Return the vowels of a string
-- c) Count the number of elements returned
countVowels :: String -> Integer
countVowels str = length $
                  filter (True &&) $
                  map isVowel $
                  foldr (++) "" $
                  words str

-- Validate the word
countSyllableParts :: (Bool -> Bool) -> String -> Int
countSyllableParts filterFunc str = length $
                                    filter filterFunc $
                                    map isVowel $
                                    foldr (++) "" $
                                    words str

countVowels' = countSyllableParts ((==) True)
countConsonants = countSyllableParts ((==) False)

newtype Word' = Word' String deriving (Eq, Show)
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = result
  where
    vowels = countVowels' str
    consonants = countConsonants str
    result = if vowels > consonants then Nothing else Just $ Word' str

-- It's only natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = (+) 1 (natToInteger nat)

integerToNat :: Integer -> Maybe Nat
integerToNat int =  if int < 0
                      then Nothing
                      else Just (reduceNatToZero int)
  where reduceNatToZero 0 = Zero
        reduceNatToZero n = Succ (reduceNatToZero (n - 1))

-- Small library for Maybe

-- 1. Simple boolean checks for Maybe values.

-- isJust (Just 1) -- True
-- isJust Nothing -- False
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- isNothing (Just 1) -- False
-- isNothing Nothing -- True
isNothing :: Maybe a -> Bool
isNothing m = not $ isJust m

-- 2. The following is the Maybe catamorphism. You can turn a Maybe

-- value into anything else with this.
-- mayybee 0 (+1) Nothing -- 0
-- mayybee 0 (+1) (Just 1) -- 2

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee seed func m = if (isNothing m) then seed else compute m
  where compute (Just x) = func x

mayybee' :: b -> (a -> b) -> Maybe a -> b
mayybee' b _ Nothing = b
mayybee' _ f (Just v) = f v

-- 3. In case you just want to provide a fallback value.
-- fromMaybe 0 Nothing -- 0
-- fromMaybe 0 (Just 1) -- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe seed Nothing = seed
fromMaybe seed (Just v) = v

-- Try writing it in terms of the maybe catamorphism
fromMaybe' seed m = mayybee seed id m

-- 4. Converting between List and Maybe.

-- listToMaybe [1, 2, 3] -- Just 1
-- listToMaybe [] -- Nothing

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
-- Doesn't really specify _which_ a its interested in.
listToMaybe (head':_) = Just head'

-- maybeToList (Just 1) -- [1]
-- maybeToList Nothing -- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5. For when we just want to drop the Nothing values from our list.
-- catMaybes [Just 1, Nothing, Just 2] -- [1, 2]
-- catMaybes [Nothing, Nothing, Nothing] -- []
catMaybes :: [Maybe a] -> [a]
catMaybes lst = [a | (Just a) <- lst]

-- 6. You’ll see this called “sequence” later.
-- flipMaybe [Just 1, Just 2, Just 3] -- Just [1, 2, 3]
-- flipMaybe [Just 1, Nothing, Just 3] -- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe lst = if anyNothings
                  then Nothing
                  else Just values
  where
    anyNothings = any isNothing lst
    values = catMaybes lst

-- Small library for Either
isLeft (Left _) = True
isLeft _ = False

isRight (Right _) = True
isRight _ = False

-- 1. Try to eventually arrive at a solution that uses foldr, even if
-- earlier versions don’t use foldr.
lefts' :: [Either a b] -> [a]
lefts' = foldr (\x xs ->  if (isLeft x)
                            then (extractLeft x) : xs
                            else xs) []
  where extractLeft (Left x) = x

-- 2. Same as the last one. Use foldr eventually.
rights' :: [Either a b] -> [b]
rights' = foldr (\x xs ->  if (isRight x)
                            then (extractRight x) : xs
                            else xs) []
  where extractRight (Right x) = x

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts'', rights'')
  where lefts'' = lefts' es
        rights'' = rights' es

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' func (Right r) = Just (func r)
eitherMaybe' func _ = Nothing

-- 5. This is a general catamorphism for Either values.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToC _ (Left l) = aToC l
either' _ bToC (Right r) = bToC r

-- 6. Same as before, but use the either' function you just wrote.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' func (Right b) = Just $ func b
eitherMaybe'' _ (Left _) = Nothing

-- Unfolds

-- 1. Write the function myIterate using direct recursion. Compare
-- the behavior with the built-in iterate to gauge correctness. Do
-- not look at the source or any examples of iterate so that you
-- are forced to do this yourself.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ myIterate f (f x)

-- The following also works, but I don't understand where the list originates.
-- myIterate' f x = x : myIterate f (f x)

-- 2. Write the function myUnfoldr using direct recursion. Compare
-- with the built-in unfoldr to check your implementation. Again,
-- don’t look at implementations of unfoldr so that you figure it
-- out yourself.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f s = if (isNothing v)
                  then []
                  else [(fst t)] ++ myUnfoldr f (snd t)
  where v = f s
        t = fromJust v

-- 3. Rewrite myIterate into betterIterate using myUnfoldr. A hint —
-- we used unfoldr to produce the same results as iterate earlier.
-- Do this with different functions and see if you can abstract the
-- structure out.

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x
