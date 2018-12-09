{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Chapter9.Playground where

import           Data.Kind    (Type)
import           Data.Monoid  ((<>))
import           Data.Proxy   (Proxy (..))
import           GHC.TypeLits

-- A Binary type constructor that is polykinded in both of its parameters.
-- Goal: Build a type-safe heterogeneously-kinded linked list.
-- Will only be used in type-level
data (a :: k1) :<< (b :: k2)
infixr 5 :<< -- associate to the right.

class HasPrintf a where -- A typeclass where every instance must provide an associated type Printf a
  type Printf a :: Type -- Corresponds to the desired type of the formatting function.

  -- String - Accumulator
  -- Proxy a exists to allow Haskell to find the correct instance of HasPrintf
  -- Printf a will expand to arrow types
  format :: String -> Proxy a -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String
  format s _ = s <> symbolVal (Proxy @text)


instance (KnownSymbol text, HasPrintf a) => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a
  format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (Show param, HasPrintf a) => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  format s _ param = format (s <> show param) (Proxy @a)

-- Special for String to avoid extra double quotes from using show instance.
instance {-# OVERLAPPING #-}  HasPrintf a => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format s _ str = format (s <> str) (Proxy @a)


printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""


