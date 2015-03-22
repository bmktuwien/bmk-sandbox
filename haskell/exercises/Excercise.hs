{-# LANGUAGE ScopedTypeVariables #-}

{--
author: Kim Bong Min

This is a collection of functions, which I wrote
for exercising purposes.
The books I read for this exercise are

 * Learn You A Haskell For Great Good

 * Real World Haskell
--}


---------------------------------------------------
---------------------------------------------------
import Control.Monad.ST
import Control.Monad

import Data.Maybe
import Data.Char
import Data.List
import Data.Array.ST

import System.Random

-- 1
-- define foldl using foldr

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f acc xs = foldr (\x g -> g . flip f x) id xs acc

-- 2
-- convert a number string into a int value

asInt :: String -> Int
asInt = myfoldl (\acc d -> digitToInt d + acc * 10) 0

-- 3
-- convert a float number into a double value
asDouble :: String -> Double
asDouble s = intpart + fracpart
  where
    (ds1,_:ds2) = splitAt (fromJust $ elemIndex '.' s) s
    intpart = fromIntegral $ asInt ds1
    fracpart = fromIntegral (asInt ds2) / 10 ^ length ds2

-- 4
-- print bunch of random numbers

printRandomNumbers :: Int -> IO()
printRandomNumbers n = do
  rGen <- getStdGen
  print $ take n (randoms rGen :: [Int])

-- 5

--bubbleSort :: Ord e => [e] -> [e]
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
