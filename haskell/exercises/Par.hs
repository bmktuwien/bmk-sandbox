import           Control.Monad.Par

main = print $ runPar computation
  where
    computation = do
      var1 <- new
      var2 <- new
      fork $ put var1 (42 :: Int)
      fork $ put var2 (47 :: Int)
      (+) <$> (get var1) <*> (get var2)
