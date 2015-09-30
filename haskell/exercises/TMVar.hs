{-# LANGUAGE LambdaCase #-}

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar

data TMVar' a = TMVar' (TVar (Maybe a))

newEmptyTMVar' :: STM (TMVar' a)
newEmptyTMVar' = TMVar' <$> newTVar Nothing

takeTMVar' :: TMVar' a -> STM a
takeTMVar' (TMVar' tvar) =
  readTVar tvar >>= \case
    Nothing -> retry
    Just e  -> writeTVar tvar Nothing >>
               return e

putTMVar' :: TMVar' a -> a -> STM ()
putTMVar' (TMVar' tvar) e =
  readTVar tvar >>= \case
    Just _  -> retry
    Nothing -> writeTVar tvar (Just e)

