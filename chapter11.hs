-- A type can be thought of as an enumeration of constructors that have zero or
-- more arguments.

-- A data constructor that takes no arguments and so is called a nullary
-- constructor.

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- 1. Is Doggies a type constructor or a data constructor?
-- Doggies is a type constructor

-- 2. What is the kind of Doggies?
-- * -> *

-- 3. What is the kind of Doggies String?
-- :k Doggies => Doggies String :: *

-- 4. What is the type of Husky 10?
-- :t Husky 10 => Num a => Doggies a

-- 5. What is the type of Husky (10 :: Integer)?
-- :t Husky (10 :: Integer) => Doggies Integer

-- 6. What is the type of Mastiff "Scooby Doo"?
-- :t Mastiff "Scooby" => Doggies String

-- 7. Is DogueDeBordeaux a type constructor or a data constructor?
-- Both

-- 8. What is the type of DogueDeBordeaux?
-- doge -> DogueDeBordeaux doge

-- 9. What is the type of d = DogueDeBordeaux "doggie!"
-- DogueDeBordeaux String

data Size = Small | Medium | Large deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Small

-- 1
myCar :: Vehicle

-- 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane  _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3
getManu :: Vehicle -> Manufacturer
getManu (Car manufacturer _) = manufacturer
getManu _ = error "no manufacturer"

-- 4
-- error, per getManu

-- 5
-- create Size, add Size to Plane, add Size instance (?) to doge

-- Exercises: Cardinality
data PugType = PugData -- 0

-- 2. For this one, recall that Bool is also defined with the |:
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited -- 3

-- 3. Given what we know about Int8, what’s the cardinality of Int16?
-- 65536

-- 4. Use the REPL and maxBound and minBound to examine Int and Integer. What
-- can you say about the cardinality of those types?
-- Int => 18446744073709551616
-- Integer => No instance for (Bounded Integer)

-- 5. Extra credit (impress your friends!): What’s the connection between the
-- 8 in Int8 and that type’s cardinality of 256?
-- Not sure. If it was Int16, the connection would be:
-- 16 bytes => 128 bits => 128 * 2 => 256
-- Thanks to: https://github.com/nackjicholson/haskellbook-solutions/blob/master/chapter11/exercises.md
-- 2 ^ 8 => 256

-- Exercises: For Example
data Example = MakeExample deriving Show

-- 1. What is the type of data constructor MakeExample? What happens when you
-- request the type of Example?
-- :t MakeExample => MakeExample :: Example
-- :t Example => Not in scope: _data constructor_ 'Example'

-- 2 What if you try :info on Example in GHCi? Can you determine
-- what typeclass instances are defined for the Example type using :info in GHCi?
-- :i Example => Show

-- 3. Try making a new datatype like Example but with a single type
-- argument added to MakeExample, such as Int. What has changed
-- when you query MakeExample with :type in GHCi?

data MyType = MyType Int
-- :t MyType => MyType :: Int -> GHCxxx.MyType

-- Exercises: Logic Goats

-- 1. Reusing the TooMany typeclass, write an instance of the typeclass for the
-- type (Int, String). This will require adding a language pragma named
-- FlexibleInstances 4 if you do not use a newtype — GHC will tell you what to
-- do.
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- (43::Int, "wut") -- True
-- (41::Int, "wut") -- False

-- 2 Make another TooMany instance for (Int, Int). Sum the values
-- together under the assumption this is a count of goats from two fields

instance TooMany (Int, Int) where
  tooMany (n, nn) = sum > 42
    where sum = n + nn

tooMany (42::Int, 1::Int) -- True

-- 3 - Make another TooMany instance, this time for (Num a, TooMany a)
-- => (a, a). This can mean whatever you want, such as summing
-- the two numbers together.

-- {-# LANGUAGE FlexibleInstances #-}

-- module TooMany3 where

--   class TooMany a where
--     tooMany :: a -> Bool

--   instance TooMany Int where
--     tooMany n = n > 42

--   instance (Num a, TooMany a) => TooMany (a, a) where
--     tooMany (n, nn) = tooMany (n + nn)

-- Exercises: Pity the Bool
-- 1
-- What is the cardinality of BigSmall? Hint: We already know
-- Bool’s cardinality. Show your work as demonstrated earlier.
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
-- 4?

-- 2
-- Example use of Numba, parentheses due to
-- syntactic collision between (-) minus and
-- the negate function

-- What is the cardinality of NumberOrBool?
-- 258 = 2 (bool) + 256 Int8

-- What happens if you try to create a Numba with a numeric literal larger than
-- 127? And with a numeric literal smaller than (-128)?
-- OOB error

import Data.Int
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)
myNumba = Numba (-128)

data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)
data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)
type TwoTwoQs = (QuantumBool, QuantumBool)

foo :: TwoTwoQs -> QuantumBool
foo (QuantumBoth, QuantumBoth) = QuantumBoth
foo (_, _) = QuantumFalse

-- data Person = MkPerson String Int deriving (Eq, Show)

data Person = Person { name :: String
                     , age :: Int } deriving (Eq, Show)

papu = Person "Papu" 5
name papu -- "Papu"
age papu -- 5

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show
type AuthorName = String
data Author = Author (AuthorName, BookType)

-- type AuthorName = String
-- data Author = Fiction AuthorName | Nonfiction AuthorName deriving (Eq, Show)

-- Exercises: How Does Your Garden Grow?

data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show
type Gardener = String
data Garden = Garden Gardener FlowerType deriving Show

-- 1. What is the normal form of Garden?
Garden = Gardener Gardenia
       | Gardener Lilac
       | Gardener Rose
       | Gardener Daisy


data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a
                                       , psecond :: b } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

-- newtype can only wrap a single constructor Some foldTree but not Some Thing Else
-- type is just a synonym - doesn't create new type e.g. type String = [Char]

-- Exercise: Programmers
-- Write a function that generates all possible values of Programmer. Use the
-- provided lists of inhabitants of OperatingSystem and ProgrammingLanguage.

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = os, lang = lang} | os <- allOperatingSystems,
                                                      lang <- allLanguages]

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show
data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show -- Sum
data Farmer = Farmer Name Acres FarmerType deriving Show -- Product

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

-- Exercise: The Quad
data Quad = One | Two | Three | Four deriving (Eq, Show)

-- 1
-- eQuad :: Either Quad Quad
-- 8

-- 2
-- prodQuad :: (Quad, Quad)
-- 16

-- 3
-- funcQuad :: Quad -> Quad
-- 256

-- 4
-- prodTBool :: (Bool, Bool, Bool)
-- 8

-- 5
-- gTwo :: Bool -> Bool -> Bool
-- 16

-- 6
-- fTwo :: Bool -> Quad -> Quad
-- 4 ^ (4 * 2) => 65536

-- Kinds are the types of type constructors, primarily encoding the number of
-- arguments they take.

-- Kinds are not types until they are fully applied.

-- The kind * -> * is waiting for a single * before it is fully applied.
-- The kind * -> * -> * must be applied twice before it will be a real type.

-- But in Haskell, we do not conventionally put constraints on datatypes. That
-- is, we don’t want to constrain that polymorphic a in the datatype.
-- The FromJSON typeclass will likely (assuming that’s what is needed in a
-- given context) constrain the variable in the type signature(s) for the
-- function(s) that will process this data.

-- All infix data constructors must start with a colon.


-- :t 1 :&: 2 => 1 :&: 2 :: (Num a, Num b) => Product a b
data Product a b = a :&: b deriving (Eq, Show)

-- type constructors are functions one level up, structuring things that cannot
-- exist at runtime — it’s purely static and describes the structure of your
-- types.

data BinaryTree a = Leaf
                    | Node (BinaryTree a) a (BinaryTree a)
                      deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

insert' 2 Leaf
insert' 1000 n


-- Write map for BinaryTree

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected
          then print "yup okay!"
          else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (inorder left) ++ (inorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (inorder left) ++ (inorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

-- Using Monoid probably counts as cheating, but I really wanted to break the
-- different computations out into smaller, named pieces. If anyone happens to
-- see this and knows of a way to somehow compose the two `b`s
-- (left' and right'), I'd be very interesting in hearing about it!
foldTree :: (Monoid a, Monoid b) => (a -> b -> b) -> b -> BinaryTree a -> b
foldTree func seed Leaf = seed
foldTree func seed (Node left a right) = func a memo
  where
    left' = (foldTree func seed left)
    right' = (foldTree func seed right)
    memo = mappend left' right'

foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' func seed Leaf = seed
foldTree' func seed (Node left a right) = foldTree' func seed' right
  where seed' = func a $ foldTree' func seed left

-- Chapter Exercises

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday

-- 1
-- a / Weekday is a type with 5 data constructors

-- 2
f Friday = "Miller Time"
-- c = f :: Weekday -> String

-- 3
-- c / types defined using `data` must begin with a capital letter

-- 4
g xs = xs !! (length xs - 1)
-- c / g delivers the final element of xs

-- Vigenère cipher

-- It looks like there's an off-by-one bug in my caeser cipher, so I'm
-- borrowing @dwayne's
-- github.com/dwayne/haskell-programming/blob/9af6fc2a106640b8f4eb7504f29fcffb89dd071d/ch9/Cipher.hs
-- The following only works on lowercase strings without spaces. Sorry.

import Data.Char

encode :: Char -> Int
encode ch = ord ch - ord 'a'

decode :: Int -> Char
decode n = chr (ord 'a' + n)

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f n ch = decode $ mod (f (encode ch) n) 26

rightShift :: Int -> Char -> Char
rightShift = shift (+)

leftShift :: Int -> Char -> Char
leftShift = shift (-)

e :: Int -> String -> String
e n = map (rightShift n)

d :: Int -> String -> String
d n = map (leftShift n)

crypt string keyword direction = concatMap func sks
  where base = ord 'a'
        offset = (\x -> (ord x) - base)
        func = (\(s, k) -> direction (offset k) [s])
        keywords = take (length string) $ cycle keyword
        sks = zip string keywords

encrypt string keyword = crypt string keyword e

decrypt string keyword = crypt string keyword d
