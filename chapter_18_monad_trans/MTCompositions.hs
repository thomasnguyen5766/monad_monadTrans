{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Writer
import Control.Monad.State
import MaybeT

problem :: (MonadWriter [String] m, MonadFail m) => m ()
problem = do
  tell ["this is where i fail"]
  fail "oops"


type A = WriterT [String] Maybe
type B = MaybeT (Writer [String])


a :: A ()
a = problem

b :: B ()
b = problem