module Playground where

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 ia) = T1 (f . ia)

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap f (T5 aii) = T5 (\bi -> aii (bi . f))
