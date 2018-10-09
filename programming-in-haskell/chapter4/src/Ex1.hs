module Ex1 where

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve (x1:[]) = ([], [])
halve (x1:x2:xs) = ((x1 : fst result), (x2 : snd result))
  where
    result = halve xs
