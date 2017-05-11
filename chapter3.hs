-- -- -- 1
-- -- False
-- -- (++) [1, 2] [3, 4]

-- -- -- 2
-- -- False
-- -- "<3" ++ " Haskell"

-- -- -- 3
-- -- True
-- -- concat ["<3", " Haskell"]

-- -- module Print3Flipped where
-- -- myGreeting :: String
-- -- myGreeting = (++) "Hello" " World"

-- -- hello :: String
-- -- hello = "hello"

-- -- world :: String
-- -- world = "world"

-- -- main :: IO ()
-- -- main = do
-- --   putStrLn myGreeting
-- --   putStrLn myGreeting'
-- --   where myGreeting' = (++) "hello" ((++) " " world)

-- -- 1
-- -- a
-- True
-- concat [[1, 2], [3, 4]]

-- -- b
-- False
-- (++) [1, 2] [3, 4]

-- -- c
-- True
-- (++) "Hello" " world"

-- -- d
-- True
-- ["hello" ++ " " ++ "world"]

-- -- e
-- False
-- "hello" !! 4

-- -- f
-- True
-- (!!) "hello" 4

-- -- g
-- False
-- take 4 "lovely"

-- -- h
-- True
-- take 3 "lovely"

-- -- 2
-- a == d
-- b == c
-- c == e
-- d == a
-- e == b

-- -- 1
-- -- a
-- excited m = (++) m "!"
-- excited "Curry is awesome"

-- -- b
-- selectY = head . drop 4
-- selectY "curry is awesome!"

-- -- c
-- lastWord = (drop 9)
-- lastWord "curry is awesome!"

-- -- 3
-- thirdLetter :: [Char] -> Char
-- thirdLetter m = m !! 2
-- thirdLetter "xyz"

-- letterIndex :: Int -> Char
-- letterIndex = (!!) "Curry is awesome!"

module Rvrse where

rvrse = c ++ " " ++ b ++ " " ++ a
  where str = "Curry is awesome!"
        a = take 5 str
        b = take 2 $ drop 6 str
        c = drop 9 str


main = print $ rvrse
