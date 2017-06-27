- algebras can be implemented as typeclasses; the typeclasses define the set
of operations
- laws make up what algebras are

-- Optional Monoid
data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) Nada = Only a
  mappend Nada (Only b) = Only b
  mappend (Only a) (Only b) = Only (mappend a b)

(Only (Sum 1) `mappend` Only (Sum 1)) == eOnly (Sum {getSum = 2})) -- True
(Only (Product 4) `mappend` Only (Product 2)) == (Only (Product {getProduct = 8})) -- True
(Only (Sum 1) `mappend` Nada) == (Only (Sum {getSum = 1})) -- True
(Only [1] `mappend` Nada) == (Only [1]) -- True
(Nada `mappend` Only (Sum 1)) == (Only (Sum {getSum = 1})) -- True

-- Associativity simply says that you can associate the arguments
-- of your operation differently and the result will be the same.

-- Commutative means you can reorder the arguments and still get the same
-- result.

-- Addition and multiplication are commutative, but (++) for the list type
-- is only associative.

-- Monoid abides by the law of associativity but not the law of commutativity,
-- even though some monoidal operations (addition and multiplication) are commutative.

-- Within an expression containing two or more occurrences in a row of the same
-- associative operator, the order in which the operations are performed does not
-- matter as long as the sequence of the operands is not changed. That is,
-- rearranging the parentheses in such an expression will not change its value.
-- Consider the following equations:

-- ( 2 + 3 ) + 4 = 2 + ( 3 + 4 ) = 9
-- 2 × ( 3 × 4 ) = ( 2 × 3 ) × 4 = 24

-- the binary operation must be associative and it must have a sensible identity value.

-- Madlibs

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat
    [
      e,
      "! he said ",
      adv,
      " as he jumped into his car ",
      noun,
      " and drove off with his ",
      adj,
      " wife."
    ]
