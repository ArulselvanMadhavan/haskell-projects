module Quicksort where

qsortR :: (Num a, Ord a) => [a] -> [a]
qsortR [] = []
qsortR (x:xs) = qsortR larger ++ [x] ++ qsortR smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]

qsort :: (Num a, Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]
