{-# LANGUAGE RankNTypes #-}
module Chapter6 where
import           Control.Monad.Trans.Class
applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

ex_6_3_1 :: Int -> forall a. a -> a -- rank 1
ex_6_3_1 = undefined

ex_6_3_2 :: (a -> b) -> (forall c. c -> a) -> b -- rank 2
ex_6_3_2 _ _ = undefined

ex_6_3_3 :: ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a -- rank 3
ex_6_3_3 _ = undefined

cont :: a -> (forall r. (a -> r) -> r)
cont a = \callback -> callback a

-- runCont :: (forall r. (a -> r) -> r) -> a --rank 2
-- runCont f =
--   let callback = id
--       in f callback

runCont :: Cont a -> a
runCont (Cont arr) = arr id

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

instance Functor Cont where
  fmap f (Cont callback) = Cont (\br -> br (f (callback id)))

instance Applicative Cont where
  pure a = Cont (\ar -> ar a)
  Cont frr <*> carr = fmap (frr id) carr

instance Monad Cont where
  return = pure
  Cont arr >>= fcbrr = fcbrr $ arr id

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r}

instance MonadTrans (ContT r) where
  lift ma = ContT (\amr -> ma >>= amr)

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 12

withOS :: (String -> r) -> r
withOS f = f "linux"

releaseString :: String
releaseString = runCont $ do
  version <- Cont withVersionNumber
  date <- Cont withTimestamp
  os <- Cont withOS
  pure $ os ++ "-" ++ show version ++ "-" ++ show date


