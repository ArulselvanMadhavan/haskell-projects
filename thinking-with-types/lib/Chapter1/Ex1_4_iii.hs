module Chapter1.Ex1_4_iii where

import           Data.Char
data Two
  = One
  | Two
  deriving (Eq, Show)

twoToString :: Two -> String
twoToString = show

lhs :: (Bool -> (Two -> String))
lhs True  = twoToString
lhs False = map toLower . twoToString

rhs :: (Bool, Two) -> String
rhs (True, t)  = twoToString t
rhs (False, t) = map toLower . twoToString $ t
