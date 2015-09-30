{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.STM hiding (TChan)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data TChan a = TChan (TVar (TVarList a))
                     (TVar (TVarList a))

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

newTChan :: STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  readVar  <- newTVar hole
  writeVar <- newTVar hole
  return $ TChan readVar writeVar


readTChan :: TChan a -> STM a
readTChan (TChan readVar _) = do
  head <- readTVar readVar

  readTVar head >>= \case
    TNil -> retry -- nothing to read yet, wait
    TCons a as -> writeTVar readVar as >>
                  return a

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ writeVar) val = do
  newEnd <- newTVar TNil
  oldEnd <- readTVar writeVar

  writeTVar writeVar newEnd
  writeTVar oldEnd (TCons val newEnd)
