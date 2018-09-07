module Product where

product' :: (Num a) => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

product'' :: (Num a) => [a] -> a
product'' = foldr (*) 1

