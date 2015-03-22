import Control.Monad.ST
import Control.Monad

import qualified Data.ByteString as B
import Data.List
import Data.Array.ST

import System.Random



bubbleSort xs = runST $ bubbleSortST xs

bubbleSortST :: [Int] -> ST s [Int]
bubbleSortST xs = do
  arr <- newListArray (0,l) xs :: ST s (STUArray s Int Int)
  mapM_ (pass arr) [0..l-1]
  getElems arr

  where
    l = length xs - 1

    swap arr i j = do
      a <- readArray arr i
      b <- readArray arr j
      writeArray arr i b
      writeArray arr j a

    step arr i j = do
      a <- readArray arr i
      b <- readArray arr j

      unless (a <= b) $ swap arr i j

    pass arr i = mapM_ (step arr i) [i+1..l]


bubbleSortPure :: [Int] -> [Int]
bubbleSortPure [] = []
bubbleSortPure xs = m : (bubbleSortPure xs')
  where
    m = minimum xs
    xs' = delete m xs

main = do
  let testData = [10^4,10^4-1..0]

  putStrLn "Testing bubble sort st"

  print $ bubbleSort testData

  putStrLn "Testing bubble sort pure"

  print $ bubbleSortPure testData
