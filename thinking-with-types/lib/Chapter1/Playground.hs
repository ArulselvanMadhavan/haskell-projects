module Chapter1.Playground where

hello :: String
hello = "Hello World!"

data Spin = Up | Down

boolToSpin1 :: Bool -> Spin
boolToSpin1 False = Up
boolToSpin1 True  = Down

-- Proving a * 1 = a
prodUnitTo :: a -> (a, ())
prodUnitTo a = (a, ())

prodUnitFrom :: (a, ()) -> a
prodUnitFrom (a, ()) = a
