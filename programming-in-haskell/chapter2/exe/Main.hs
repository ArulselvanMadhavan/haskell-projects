module Main where

init' :: [a] -> [a]
init' []     = []
init' (x:[]) = []
init' (x:xs) = x : (init' xs)

init'' :: [a] -> [a]
init'' xs = take n_minus_1 xs
  where
    n_minus_1 = length xs - 1

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn (show $ init' [1,2,3,4])
  putStrLn (show $ init'' [1,2,3,4])
