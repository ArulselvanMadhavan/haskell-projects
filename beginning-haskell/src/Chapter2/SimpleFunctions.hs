module Chapter2.SimpleFunctions where

firstOrEmpty :: [String] -> String
firstOrEmpty xs =
  if not (null xs) then head xs else "empty"
