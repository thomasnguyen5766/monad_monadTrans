module StateT (
  StateT(..)
  ) where

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

flipmap :: (a -> b) -> (,) a s -> (,) b s
flipmap f (a, s) = (f a, s)

instance Functor m => Functor (StateT s m) where
  -- fmap :: (a -> b) -> StateT s m a -> StateT s m b
  -- fmap f (StateT sma) = StateT $ \s -> (fmap . flipmap) f (sma s)
  fmap f sma = StateT $ \s -> fmap (\(a, s') -> (f a, s'))  $  runStateT sma s 

instance (Monad m, Applicative m) => Applicative (StateT s m) where
  -- pure :: a -> StateT s m a
  pure a = StateT $ \s -> return (a ,s)

  -- <*> :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT smf <*> StateT sma = 
    StateT $ \s -> do
     (f, s') <- smf s
     (a, s'') <- sma s'
     return (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure

  -- (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= f = 
    StateT $ \s -> do
      (a, s') <- sma s
      runStateT (f a) s'

{-
  to strip out the type m - from m (a, s) - remember - m must already a monad
  - we can use `do` notation or (>>=) operator to do that. 
-}

{-
  In functor instance implementation -  we can do this to get the inside tuple
  -- But we don't need this - we can `lift` function to the context
  -- Strip the monad skin is expensive computation - avoid it when we can
-}