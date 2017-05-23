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
