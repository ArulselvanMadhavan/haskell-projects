{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Chapter10.FCF where
import           Data.Kind (Constraint, Type)

-- Type-Level Defunctionalization
-- First Class Families

type Exp a = a -> Type -- Type level function

-- Open type family
type family Eval (e :: Exp a) :: a

-- "Defunctionalized Labels" with Empty data types
data Snd :: (a, b) -> Exp b

type instance Eval (Snd '(a, b)) = b

-- "Defunctionalized pattern matching functions"
data FromMaybe :: a -> Maybe a -> Exp a

type instance Eval (FromMaybe _1 ('Just a)) = a
type instance Eval (FromMaybe a 'Nothing) = a

-- Ex 10.2-i

data ListToMaybe :: [a] -> Exp (Maybe a)

type instance Eval (ListToMaybe '[]) = 'Nothing
type instance Eval (ListToMaybe (x ': xs)) = 'Just x

-- FCF for higher order functions.

data MapList :: (a -> Exp b) -> [a] -> Exp [b]

type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (x ': xs)) = Eval (f x) ': Eval (MapList f xs)

data Foldr :: (a -> Exp b -> Exp b) -> Exp b -> [a] -> Exp b

type instance Eval (Foldr f b '[]) = Eval b
type instance Eval (Foldr f b (x ': xs)) = Eval (f x (Foldr f b xs))

