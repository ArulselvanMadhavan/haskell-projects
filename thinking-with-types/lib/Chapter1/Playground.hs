module Chapter1.Playground where

hello :: String
hello = "Hello World!"

data Void
data Spin = Up | Down

boolToSpin1 :: Bool -> Spin
boolToSpin1 False = Up
boolToSpin1 True  = Down

-- Proving a * 1 = a
prodUnitTo :: a -> (a, ())
prodUnitTo a = (a, ())

prodUnitFrom :: (a, ()) -> a
prodUnitFrom (a, ()) = a

absurd :: Void -> a
absurd _ = undefined :: a

sumUnitTo :: Either a Void -> a
sumUnitTo (Left a)  = a
sumUnitTo (Right v) = absurd v

sumUnitFrom :: a -> Either a Void
sumUnitFrom = Left
