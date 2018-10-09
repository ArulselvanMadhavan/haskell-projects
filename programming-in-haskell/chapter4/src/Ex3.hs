module Ex3 where

null' :: [a] -> Bool
null' xs =
  if length xs == 0
  then True
  else False

safetail :: [a] -> [a]
safetail xs =
  if null' xs
  then []
  else tail xs
