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

-- 10.3

data Pure :: a -> Exp a
type instance Eval (Pure x) = x

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))
infixr 0 =<<

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))
infixr 1 <=<

data TyEq :: a -> b -> Exp Bool

type instance Eval (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

data Collapse :: [Constraint] -> Exp Constraint

type instance Eval (Collapse '[]) = (() :: Constraint)
type instance Eval (Collapse (x ': xs)) = (x, Eval (Collapse xs))

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x

type All (c :: k -> Constraint)(ts :: [k]) =
  Collapse =<< MapList (Pure1 c) ts

data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

type instance Eval (Map f 'Nothing) = 'Nothing
type instance Eval (Map f ('Just a)) = 'Just (Eval (f a))

type instance Eval (Map f ('Left x)) = 'Left x
type instance Eval (Map f ('Right x)) = 'Right (Eval (f x))

-- Ex 10.4-i
type instance Eval (Map f '(a, b)) = '(a, Eval (f b))

data Mappend :: a -> a -> Exp a
type instance Eval (Mappend '() '()) = '()
type instance Eval (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)
-- type instance Eval (Mappend (a :: [k]) (b :: [k])) = Eval (a ++ b)

-- Type families are not allowed to discriminate on their return type.
-- Cheat this restriction by muddying up the interface a little and making
-- the type application explicit.

data Mempty :: k -> Exp k

type instance Eval (Mempty '()) = '()
type instance Eval (Mempty (c :: Constraint)) = (() :: Constraint)
type instance Eval (Mempty (l :: [k])) = '[]
