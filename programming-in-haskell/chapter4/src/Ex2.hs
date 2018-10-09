module Ex2 where

third :: [a] -> a
third (x1 : x2 : x3 : xs) = x3

third' :: [a] -> a
third' xs =
  head . tail . tail $ xs

third'' :: [a] -> a
third'' xs =
  xs !! 2
