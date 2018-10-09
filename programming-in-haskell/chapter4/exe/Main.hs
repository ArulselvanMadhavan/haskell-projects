module Main where

import           Ex1

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn . show $ halve [1, 2, 3, 4, 5, 6]
