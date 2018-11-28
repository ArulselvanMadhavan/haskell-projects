{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
module Playground where
import           Data.Functor.Foldable hiding (ListF)
import           Data.List.Ordered     (merge)
import           Prelude               hiding (Foldable, succ)

data NatF r
  = ZeroF
  | SuccF r
  deriving (Show, Functor)

data ListF a r
  = NilF
  | ConsF a r
  deriving (Show, Functor)

data TreeF a r
  = EmptyF
  | LeafF a
  | NodeF r
          r
  deriving (Show, Functor)

-- newtype Fix f = Fix (f (Fix f))

-- fix :: f (Fix f) -> Fix f
-- fix = Fix

-- unfix :: Fix f -> f (Fix f)
-- unfix (Fix f) = f

type Nat = Fix NatF
type List a = Fix (ListF a)
type Tree a = Fix (TreeF a)

zero :: Nat
zero = Fix ZeroF


succ :: Nat -> Nat
succ = Fix . SuccF

nil :: List a
nil = Fix NilF

cons :: a -> List a -> List a
cons x xs = Fix (ConsF x xs)

natsum :: Fix NatF -> Int
natsum = cata $ \case
  ZeroF     -> 0
  SuccF n -> n +1

newtype Dummy f = Dummy f deriving (Show)

-- This is the most interesting thing I learned today.
-- If you pass a value of type f a , it gets automatically inferred as f (Dummy a)
dummy :: (Functor f) => f (Dummy a) -> f (Dummy a)
dummy fd = fmap id fd

filterL :: (a -> Bool) -> List a -> List a
filterL p =
  cata $ \case
    NilF -> nil
    ConsF x xs ->
      if p x
        then cons x xs
        else xs

nat :: Int -> Nat
nat = ana $ \case
  n | n <= 0 -> ZeroF
  n | otherwise -> SuccF (n - 1)

natfac :: Nat -> Int
natfac = para alg where
  alg ZeroF          = 1
  alg (SuccF (n, f)) = natsum (succ n) * f

natpred :: Nat -> Nat
natpred = para alg where
  alg ZeroF          = zero
  alg (SuccF (n, _)) = n

tailL :: List a -> List a
tailL = para alg where
  alg NilF             = nil
  alg (ConsF _ (l, _)) = l

-- Expresses Corecursive production with recursive consumption
-- hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b

mergeSort :: Ord a => [a] -> [a]
mergeSort = hylo alg coalg where
  alg EmptyF      = []
  alg (LeafF c)   = [c]
  alg (NodeF l r) = merge l r

  coalg [] = EmptyF
  coalg [x] = LeafF x
  coalg xs = NodeF l r where
    (l, r) = splitAt (length xs `div` 2) xs

-- showF :: forall a f. (Functor f, Show a)  => Fix f -> String
-- showF ff = do
--   f <- unfix ff
--   putStrLn "dummy"
