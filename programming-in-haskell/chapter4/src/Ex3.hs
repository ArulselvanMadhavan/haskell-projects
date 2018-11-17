module Ex3 where

null' :: [a] -> Bool
null' xs =
  length xs == 0

null'' :: [a] -> Bool
null'' [] = True
null'' _  = False

null''' :: [a] -> Bool
null''' xs
  | length xs == 0 = True
  | otherwise = False

safetail :: [a] -> [a]
safetail xs =
  if null' xs
  then []
  else tail xs
