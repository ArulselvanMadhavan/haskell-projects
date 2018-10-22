module Ex3 where

null' :: [a] -> Bool
null' xs =
  if length xs == 0
  then True
  else False

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
