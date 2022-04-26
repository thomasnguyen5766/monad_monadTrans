{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}

module MaybeT (
  MaybeT(..)
  ) where

import Control.Monad.State
import Control.Monad.Writer.Class
import Control.Monad.Fail

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  -- fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance Applicative f => Applicative (MaybeT f) where
  -- pure :: a -> f a
    pure x = MaybeT $ (pure . pure) x

  -- <*> :: MaybeT f (a -> b) -> MaybeT f a -> MaybeT f b
    MaybeT fab <*> MaybeT fa = MaybeT $ (<*>) <$> fab <*> fa

instance Monad m => Monad (MaybeT m) where
  return = pure
  -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  -- MaybeT mma >>= f = MaybeT $ mma >>= \x -> case x of
  --                                             Nothing -> return Nothing
  --                                             (Just y) -> runMaybeT (f y)
  MaybeT mma >>= f = MaybeT $ do
    v <- mma
    case v of
      Nothing -> return Nothing
      (Just y) -> runMaybeT (f y)

  


instance MonadTrans MaybeT where
  lift m = MaybeT (Just `liftM` m)

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put k = lift (put k)

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO m = lift (liftIO m)

instance (MonadWriter w m) => MonadWriter w (MaybeT m)  where
  -- tell :: w -> m ()
  writer = lift . writer
  tell   = lift . tell

-- instance (Monad m) => MonadFail m where
--   fail _= MaybeT $ return Nothing 

instance (Monad m) => MonadFail (MaybeT m) where
  fail _ = MaybeT $ return Nothing 
