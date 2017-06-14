type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                                              "Name was: " ++ show name ++
                                              " Age was: " ++ show age


-- a) It should prompt the user for a name and age input.
-- b) It should attempt to construct a Person value using the
-- name and age the user entered. You’ll need the read func-
-- tion for Age because it’s an Integer rather than a String.
-- c) If it constructed a successful person, it should print ”Yay!
-- Successfully got a person:” followed by the Person value.
-- d) If it got an error value, report that an error occurred and
-- print the error.

handlePersonCreationError :: PersonInvalid -> IO ()
handlePersonCreationError error =
  putStrLn $ "Whoops! Something went wrong: " ++ (show error)

handlePersonCreationSuccess :: Person -> IO ()
handlePersonCreationSuccess person =
  putStrLn $ "Yay! Successfully got a person: " ++ (show person)

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn "What is your age?"
  age <- getLine
  let age' = readMaybe age :: Maybe Integer
  if (isNothing age')
    then handlePersonCreationError (PersonInvalidUnknown "Parse Error")
    else
      case (mkPerson name (fromJust age')) of
        (Left error) -> handlePersonCreationError error
        (Right success) -> handlePersonCreationSuccess success
