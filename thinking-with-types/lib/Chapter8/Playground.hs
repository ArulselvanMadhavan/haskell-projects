{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Chapter8.Playground where
import           Data.Coerce   (Coercible (..), coerce)
import           Data.Foldable (toList)
import qualified Data.Map      as M
import           Data.Monoid   (Product (..), Sum (..))

newtype ZipList a = ZipList {getZipList :: [a]}
-- newtype Sum a = Sum {getSum :: a}

slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce


newtype Reverse a = Reverse
  { getReverse :: a
  } deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
  compare (Reverse a) (Reverse b) = compare b a

-- Role signature of Either a b is Either Repr Repr
-- Role signature of Proxy a is Proxy Phantom

type family IntToBool a where
  IntToBool Int = Bool
  IntToBool a = a

data BST v
  = Empty
  | Branch (BST v)
           (BST v)
  deriving (Eq, Show)

type role BST nominal
