{-# LANGUAGE GADTs #-}
module Chapter5.Playground where

sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA = undefined

five :: Int
five = 5

five_ :: (a ~ Int) => a
five_ = 5

-- GADT syntax
data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt i)  = i
evalExpr (LitBool b) = b
evalExpr (Add x y)   = evalExpr x + evalExpr y
evalExpr (Not x) = not $ evalExpr x
evalExpr (If b t f) =
  if evalExpr b
    then evalExpr t
    else evalExpr f

data Expr_ a
  = (a ~ Int) => LitInt_ Int
  | (a ~ Bool) => LitBool_ Bool
  | (a ~ Int) => Add_ (Expr_ Int) (Expr_ Int)
  | (a ~ Bool) => Not_ (Expr_ Bool)
  | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

