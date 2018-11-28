{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
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

-- natsum :: Fix NatF -> Int
-- natsum = cata $ \case
--   ZeroF     -> 0
--   SuccF n -> n +1

newtype Dummy f = Dummy f deriving (Show)

-- This is the most interesting thing I learned today.
-- If you pass a value of type f a , it gets automatically inferred as f (Dummy a)
dummy :: (Functor f) => f (Dummy a) -> f (Dummy a)
dummy fd = fmap id fd

