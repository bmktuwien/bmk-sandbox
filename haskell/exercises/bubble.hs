import Control.Monad.ST
import Data.Array.ST
import Control.Monad
import Criterion.Main
import Data.List

bubbleSort :: [Int]->[Int]
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

bubbleSort' :: [Int] -> [Int]
bubbleSort' [] = []
bubbleSort' xs = m : (bubbleSort' xs')
 where
   m = minimum xs
   xs' = delete m xs


minimumBmk :: [Int] -> Int
minimumBmk = minimum

minimumBernhard :: [Int] -> Int
minimumBernhard = head . sort

{--
main :: IO ()
main = defaultMain
       [ bgroup "id" [ bench "e3" $ nf id l1
                     , bench "e4" $ nf id l2
                     , bench "e5" $ nf id l3
                     , bench "e6" $ nf id l4
                     ]
       , bgroup "bsort-st" [ bench "e3" $ nf bubbleSort l1
                           , bench "e4" $ nf bubbleSort l2
                           , bench "e5" $ nf bubbleSort l3
                           , bench "e6" $ nf bubbleSort l4
                           ]
       , bgroup "bsort-pure" [ bench "e3" $ nf bubbleSort' l1
                             , bench "e4" $ nf bubbleSort' l2
                             , bench "e5" $ nf bubbleSort' l3
                             , bench "e6" $ nf bubbleSort' l4
                             ]
       , bgroup "sort" [ bench "e3" $ nf sort l1
                       , bench "e4" $ nf sort l2
                       , bench "e5" $ nf sort l3
                       , bench "e6" $ nf sort l4
                      ]
       ]

  where
    l1 = lst 10
    l2 = lst 100
    l3 = lst 1000
    l4 = lst (10^4)

    lst x = [x,x-1 .. 1]
--}


main :: IO ()
main = defaultMain
       [ bgroup "minimum-bmk" [ bench "e3" $ nf minimumBmk l1
                     , bench "e4" $ nf minimumBmk l2
                     , bench "e5" $ nf minimumBmk l3
                     , bench "e6" $ nf minimumBmk l4
                     , bench "e7" $ nf minimumBmk l5
                     ]
       , bgroup "minimum-bernhard" [ bench "e3" $ nf minimumBernhard l1
                                   , bench "e4" $ nf minimumBernhard l2
                                   , bench "e5" $ nf minimumBernhard l3
                                   , bench "e6" $ nf minimumBernhard l4
                                   , bench "e7" $ nf minimumBernhard l5
                                   ]
       ]

  where
    l1 = lst 10
    l2 = lst 100
    l3 = lst 1000
    l4 = lst 10000
    l5 = lst 100000

    lst x = [0 .. x]
