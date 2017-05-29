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
