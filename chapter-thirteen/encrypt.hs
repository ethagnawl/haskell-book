module Encrypt where

import Cipher

main :: IO ()
main = do
  putStrLn "Enter the key you'd like to use to encrypt your message."
  key <- getLine
  putStrLn "Enter the message you'd like to encrypt."
  wordToEncrypt <- getLine
  let encryptedWord = encrypt wordToEncrypt key
  putStrLn encryptedWord
