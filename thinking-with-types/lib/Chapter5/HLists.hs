{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Chapter5.HLists where
import           Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where -- GADTs
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

-- Three element list whose second element is boolean
showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

-- instance Eq (HList '[]) where
--   HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--   (a :# as) == (b :# bs) = a == b && as == bs

-- instance Show (HList '[]) where
--   show HNil = "HNil"

-- instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
--   show (a :# as) = show a ++ " " ++ show as

-- instance Ord (HList '[]) where
--   compare HNil HNil = EQ

-- instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--   compare (a :# as) (b :# bs) = case compare a b of
--                                   EQ -> compare as bs
--                                   x@_ -> x

-- Type family to enforce that all elements of HList has an Eq instance.
type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

-- Make the above type family generic and work with any constraint.
type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (a :# as) = show a ++ " " ++ show as

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (a :# as) (b :# bs) = case compare a b of
                                  EQ -> compare as bs
                                  x@_ -> x
