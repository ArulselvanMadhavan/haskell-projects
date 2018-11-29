{-# LANGUAGE DeriveFunctor #-}
module SumTypeOfWay where
import           Control.Category
data ExprF a
  = Index a
          a
  | CallF [a]
  | UnaryF String
           a
  | BinaryF a
            String
            a
  | ParenF a
  | LiteralF
  deriving (Show, Eq, Functor)


newtype Term f = In
  { out :: f (Term f)
  }
type Expr' = Term ExprF

topDown, bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn = out >>> fmap (bottomUp fn) >>> In >>> fn
topDown fn = In <<< fmap (topDown fn) <<< out <<< fn

flattenTerm :: Expr' -> Expr'
flattenTerm (In (ParenF e)) = e
flattenTerm o               = o

flatten'' :: Expr' -> Expr'
flatten'' = bottomUp flattenTerm
