module CountEntriesT (listDirectory, countEntries) where

import CountEntries (listDirectory)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)

{-
  listDirectory :: FilePath -> IO [String]

  liftIO :: MonadIO m => IO a -> m a
  
  --liftIO . listDirectory :: FilePath -> m [String]

  tell ::  MonadWriter w m => w -> m ()

  forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()

  when :: Applicative f => Bool -> f () -> f ()
  when b m = if b then m else mempty

-}

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName
