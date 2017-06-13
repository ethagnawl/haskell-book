module Decrypt where

import Cipher

main :: IO ()
main = do
  putStrLn "Enter the key you'd like to use to decrypt your message."
  key <- getLine
  putStrLn "Enter the message you'd like to decrypt."
  wordToDecrypt <- getLine
  let decryptedWord = decrypt wordToDecrypt key
  putStrLn decryptedWord
