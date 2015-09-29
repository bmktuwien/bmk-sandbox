{-# LANGUAGE ScopedTypeVariables #-}

import Data.Unique
import Control.Exception
import Control.Concurrent

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

        handle toHandler $ bracket
          (forkIO (threadDelay to >> throwTo tid toe))
          (\wTid -> killThread wTid)
          (\_ -> fmap Just action)
