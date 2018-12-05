{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Playground where
import           Data.Foldable    (asum)
import           Data.IORef
import           Data.Kind        (Constraint, Type)
import           Data.Maybe
import           Data.Typeable
import           System.IO.Unsafe (unsafePerformIO)
-- Any type doesn't save type information in the type constructor.
-- So it loses all the type information.
data Any where
  Any :: a -> Any

-- Existential types can be eliminated using Continuation Passing style.
elimAny :: (forall a. a -> r) -> Any -> r -- rank 2
elimAny f (Any a) = f a

--ex_7_1_i
--functions of type forall a. a -> r are interesting because
--they enable CPS style. They accept any type as input argument
--and allow the caller to pick the argument type.

-- Packing a typeclass dictionary along with the existentialized type.
data HasShow where -- GADT
  HasShow :: Show t => t -> HasShow -- packs a type class with the existentialized type.

instance Show HasShow where
  show (HasShow s) = "HasShow " ++ show s -- Show instance of the existentialized type is available.

elimHasShow :: (forall a. Show a => a -> r)  -> HasShow -> r
elimHasShow f (HasShow a) = f a

--ex7_1_ii
--If you remove the show instance from the existentialized type
--you will not be able to use the show instance of the existentialized type in the Show instance for HasShow.

--ex7_1_iii
-- instance Show HasShow where
--   show (HasShow s) = elimHasShow (\et -> "HasShow " ++ show et)

                            -- Dynamic Types
-- Typeable class is more interesting to pack with an existentialized type.

data Dynamic where -- GADT
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic t) = f t

fromDynamic :: forall a. Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r) => Dynamic -> Dynamic -> (a -> b -> r) -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $ asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int @Int a b (+)
    , liftD2 @String @Int a b $ \strA intB ->
        strA ++ show intB
    , liftD2 @Int @String a b $ \intA strB ->
        show intA ++ strB
    ]
-- Dynamically typed languages are strongly types languages with a single type.

-- Generalized Constraint Kinded Existentials

data Has (c :: Type -> Constraint) where -- Polymorphic over constraints.
  Has :: c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

type HasShow' = Has Show
type HasDynamic' = Has Typeable

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty a = a == mempty

-- There is no type-level lambda syntax.
-- So, curried type-level application is not possible.

-- Type Synonyms must always be fully saturated.

-- Constraint Synonyms
class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a -- Needs FlexibleInstances and UndecidableInstances.

-- Scoping Information with Existentials
-- Used to prevent information from leaking outside of a desired scope.
-- Existential types are unable to exist outside of their quantifier.
-- Tag sensitive information with existential types.
-- Type system can prevent them from being moved outside of the scope.
-- Example: ST monad

newtype ST s a = ST { unsafeRunST :: a } -- s is a phantom type parameter. Used to put our existential type tag.

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST a) where
  pure = ST
  ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
  ST a >>= f = seq a $ f a
