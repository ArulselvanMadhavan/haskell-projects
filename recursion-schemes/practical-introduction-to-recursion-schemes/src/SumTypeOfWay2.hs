{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}
import           Control.Arrow
import           Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

data Expr a
  = Literal { intVal :: Int }
  | Ident { name :: String }
  | Index { target :: a
          , idx    :: a }
  | Unary { op     :: String
          , target :: a }
  | Binary { lhs :: a
           , op  :: String
           , rhs :: a }
  | Call { func :: a
         , args :: [a] }
  | Paren { target :: a }
  deriving (Eq, Show, Functor)

newtype Term f = In
  { out :: f (Term f)
  }

ten, add, call :: Term Expr
ten = In (Literal {intVal = 10})
add = In (Ident {name = "add"})
call = In (Call {func = add, args = [ten, ten]})

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
  out
  >>> fmap (bottomUp fn)
  >>> In
  >>> fn

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out >>> fmap (cata fn) >>> fn

countNodes :: Expr Int -> Int
countNodes (Unary _ arg)         = arg + 1
countNodes (Binary left _ right) = left + 1 + right
countNodes (Call fn args)        = fn + sum args + 1
countNodes (Index it idx)        = it + idx + 1
countNodes (Paren arg)           = arg + 1
countNodes (Literal _)           = 1
countNodes (Ident _)             = 1

prettyPrint :: Algebra Expr Doc
prettyPrint (Literal i)    = P.int i
prettyPrint (Ident s)      = P.text s
prettyPrint (Call f as)    = f <> (P.parens $ P.fcat (P.punctuate (P.ptext ",") as))
prettyPrint (Index it idx) = it <> P.brackets idx
prettyPrint (Unary op it) = (P.text op) <> it
prettyPrint (Binary l op r) = l <> (P.text op) <> r
prettyPrint (Paren exp) = P.parens exp

given :: forall h a . (Functor h) => h a -> a
given = undefined

func' :: forall f g a. f a -> g a
func' = undefined

-- cat => cat a b -> cat b c -> cat a c
-- given >>> :: (-> b c) -> (f b -> c)
-- given >>> (fmap func') :: f (g b) -> f b
