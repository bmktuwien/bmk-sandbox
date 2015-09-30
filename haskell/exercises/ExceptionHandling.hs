import           Control.Concurrent
import           Control.Exception

barfoo :: Int -> IO ()
barfoo retryCnt
  | retryCnt < 3 = foobar (retryCnt+1)
  | otherwise = error "Giving up"

foobar :: Int -> IO ()
foobar retryCnt = action `catches` [Handler handler]
  where
    action = do
      -- check current masking state
      maskState <- getMaskingState
      print maskState

      -- Simulate long compuation.
      -- We just block here, so we can still receive async exceptions
      -- even in 'MaskedInterruptible' state
      threadDelay 100000

    handler :: SomeException -> IO ()
    handler e = do
      print $ "Got exception: " ++ show e
      -- when tail-calling directly in exception handler
      -- we inherit the state 'MaskedInterruptible',
      -- which is in most cases not what we want...
      barfoo retryCnt

main = do
  -- spawn the worker thread
  tid <- forkIO $ foobar 0

  -- wait for 1 sec
  threadDelay 1000

  killThread tid
  killThread tid
  killThread tid
  killThread tid
