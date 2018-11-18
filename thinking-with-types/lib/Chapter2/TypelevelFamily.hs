{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
module Chapter2.TypelevelFamily where

  -- Term level function
or :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
or Prelude.True _  = Prelude.True
or Prelude.False y = y

type family Or (x :: Prelude.Bool) (y :: Prelude.Bool) :: Prelude.Bool where
  Or 'True y = 'True
  Or 'False y = y

    -- Exercise 2.4.i
not :: Bool -> Bool
not True  = False
not False = True

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (a : as) = f a : Chapter2.TypelevelFamily.map f as

type family Map (f :: a -> b) (i :: [a]) :: [b] where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Foo (x::Bool) (y::Bool) :: Bool
type family Bar x y :: Bool -> Bool -> Bool
