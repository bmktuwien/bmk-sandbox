{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Concurrent.Async

waitBoth :: Async a -> Async b -> IO (a,b)
waitBoth a b =
  waitEither a b >>= \case
    Left res  -> (res,) <$> wait b
    Right res -> (,res) <$> wait a
