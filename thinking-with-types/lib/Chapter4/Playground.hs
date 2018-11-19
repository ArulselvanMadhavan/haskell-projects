{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Playground where
import           Data.Typeable

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

type family AlwaysUnit a where
  AlwaysUnit a = ()

    -- Example1
ex1 :: AlwaysUnit a -> a
ex1 _ = undefined

        -- Ex 2
ex2 :: b -> AlwaysUnit a -> b
ex2 b _ = b

          -- Ex 3
ex3 :: forall a. (Typeable a, Show a) => AlwaysUnit a -> String
ex3 _ = typeName @a

