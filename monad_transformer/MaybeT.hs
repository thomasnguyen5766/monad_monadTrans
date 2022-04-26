module MaybeT (
  MaybeT(..)
  ) where
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