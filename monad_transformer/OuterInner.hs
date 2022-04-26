module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader


embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: ()
  -> IO (Either String
            (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

instance Monad ((->) r) where
  return = const

instance Monad (Either e) where
  return = Right

instance Monad Maybe where
  return = Just

{-
  We can treat having used return for the Reader / Either / Maybe stack as composition  
-}

{-
  The outer most structure from a monad transformer --> basedmonad
-}

type MyType a = IO [Maybe a] --> IO here is based monad