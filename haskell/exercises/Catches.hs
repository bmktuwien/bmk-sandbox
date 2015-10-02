{-# LANGUAGE ExistentialQuantification #-}

import           Control.Concurrent
import           Control.Exception

data Handler' a = forall e . Exception e => Handler' ((IO a -> IO a) -> e -> IO a)

catches' :: IO a -> [Handler' a] -> IO a
catches' io handlers = mask $ \restore ->
  either (errorHandler restore handlers) return =<< try (restore io)

catchesUnmasked :: IO a -> [Handler a] -> IO a
catchesUnmasked io handlers =
  either (catchesHandler handlers) return =<< try io

catchesUnmasked' :: IO a -> [Handler a] -> IO a
catchesUnmasked' io handlers = mask $ \restore ->
  either (errorHandler' restore handlers) return =<< try (restore io)

errorHandler :: (IO a -> IO a) -> [Handler' a] -> SomeException -> IO a
errorHandler restore handlers e = foldr handle' (throw e) handlers
  where
    handle' (Handler' handler) res =
      case fromException e of
      Just e' -> handler restore e'
      Nothing -> res

errorHandler' :: (IO a -> IO a) -> [Handler a] -> SomeException -> IO a
errorHandler' restore handlers e = foldr handle' (throw e) handlers
  where
    handle' (Handler handler) res =
      case fromException e of
        Just e' -> restore (handler e') `finally`
                   putStrLn "exception handling finalizer"
        Nothing -> res

catchesHandler :: [Handler a] -> SomeException -> IO a
catchesHandler handlers e = foldr tryHandler (throw e) handlers
  where
     tryHandler (Handler handler) res =
      case fromException e of
      Just e' -> (handler e') `finally`
                 putStrLn "exception handling finalizer"
      Nothing -> res

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


foobar :: IO ()
foobar = action `catchesUnmasked` [Handler handler]
  where
    action = threadDelay 100000

    handler :: SomeException -> IO ()
    handler e =
      (print $ "Got exception: " ++ show e) `finally`
      (print "Exception processed")


main :: IO ()
main = do
  -- spawn the foobar thread
  tid <- forkIO foobar

  -- kill the thread asynchronously
  killThread tid
  killThread tid
