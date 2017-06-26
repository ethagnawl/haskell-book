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
