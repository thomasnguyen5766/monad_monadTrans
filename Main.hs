{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Data.Monoid (mconcat)

import Control.Monad.Trans.Class -- lift - to lift over Additional Structured

import Lib


{-
  lift :: IO a -> ActionM a
-}


{-
  Monad transformers can be nested in order to compose various effects into one large function
  -- To manage those stack, we need MonadTrans (lift)

  MonadTrans - typeclass 
  -- One core method : lift - lifting actions in some monad over a transformer type which wraps itself in the original Monad


  class MonadTrans t where
    lift :: (Monad m) => m a -> t m a

    -- Lift a computation from the argument monad to the constructed monad.
    t - monad transformer type - instance of MonadTrans

-}


{-
  newtype ScottyT e m a = ScottyT { runS :: State (ScottyState e m) a }
    deriving (Functor, Applicative, Monad)

  


-}

{-
newtype ActionT e m a = ActionT
  { runAM
      :: ExceptT
          (ActionError e)
          (ReaderT ActionEnv
            (StateT ScottyResponse m))
          a
  } deriving ( Functor, Applicative )

-}

{-
  type ScottyM = ScottyT Text IO
  type ActionM = ActionT Text IO
-}


{-

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    putStrLn "hello"
    html $
      mconcat ["<h1>Scotty, ",
                beam,
                " me up! <h1>"]

Get an Error:
Expected: Web.Scotty.Internal.Types.ActionT
                  text-1.2.5.0:Data.Text.Internal.Lazy.Text IO ()
        Actual: IO ()


  Explanation:
  putStrLn :: String -> IO ()
  -- PutStrLn action has the type IO (), but inside a do block of  `get` which is 
  `get` :: RoutePattern -> ActionM () -> ScottyM ()

  `param` :: ..Text -> ActionM a

  - the context is ActionM - which actually reaches IO

  To fix this, we need to lift over additial structure -> import Control.Monad.Trans.Class

-}
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word" -- :: ActionM a 
    lift $ putStrLn "hello" -- lift :: IO a -> ActionM a
    html $
      mconcat ["<h1>Scotty, ",
                beam,
                " me up! <h1>"]
