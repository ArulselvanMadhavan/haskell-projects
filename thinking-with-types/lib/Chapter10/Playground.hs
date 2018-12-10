{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
module Chapter10.Playground where
import           Data.Kind (Constraint, Type)
import           Prelude   hiding (fst)


data Fst a b = Fst (a, b)

class Eval l t | l -> t  where
  eval :: l -> t

instance Eval (Fst a b) a where
  eval (Fst (a, _)) = a

-- 10.1-i
-- listToMaybe :: [a] -> Maybe a

data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe [])      = Nothing
  eval (ListToMaybe (x : _)) = Just x

-- Defunctionalizing higher order functions.
data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList _ [])       = []
  eval (MapList f (a : as)) = eval (f a) : eval (MapList f as)

