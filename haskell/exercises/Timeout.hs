{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Data.Unique

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data Timeout = Timeout Unique
             deriving Eq

instance Show Timeout where
    show _ = "Timeout!!!"

instance Exception Timeout

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

timeout :: Int -> IO a -> IO (Maybe a)
timeout to action
 | to < 0 = fmap Just action
 | otherwise = do
  tid <- myThreadId
  toe <- fmap Timeout newUnique

  let toHandler :: Timeout -> IO (Maybe a)
      toHandler e = if e == toe then return Nothing else throw e

  -- spawn watcher Thread
  forkIO $ do
    threadDelay to
    throwTo tid toe

  handle toHandler $ bracket (forkIO (threadDelay to >> throwTo tid toe))
                             (\wTid -> killThread wTid)
                             (\_ -> fmap Just action)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

timeout' :: Int -> IO a -> IO (Maybe a)
timeout' to action
  | to < 0  = fmap Just action
  | to == 0 = return Nothing
  | otherwise = do
      race (threadDelay to) action >>= \case
        Left _    -> return Nothing
        Right res -> return $ Just res
