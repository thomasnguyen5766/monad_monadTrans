
{-
  (Reader r -- monad) && (ReaderT r m --  monad transformer)  is an instance of the MonadReader

  - r - any type - immutable state that the reader monad carries around

  class (Monad m) => MonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a


  - when the underlying monad m - is an instance of MonadIO, then
    instance (MonadIO m) => MonadIO (ReaderT r m) where
-}

{-#LANGUAGE FlexibleContexts#-}

import Control.Monad.Reader


myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)