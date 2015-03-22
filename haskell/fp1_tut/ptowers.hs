import Data.List
import Data.Maybe
import Data.Ord

type Tower = [Int]
type PerfectWorld = [Tower]


--powerof2 = 2 : map (*2) powerof2

powerof2 = [2^x | x <- [1..]]

isPowerOf2 n = n == fromJust (find (>= n) powerof2)

-- naive very ineffcient brute-force implementation
-- even solutions for 7 towers take hopelessly long
-- no wonder, cause we also consider every permutated solution...
-----------------------------------------------------------

solve n = max
  where
    all = solveAll (replicate n []) 1
    max = maximumBy (comparing (maximum . concatMap id)) all

solveAll :: PerfectWorld -> Int -> [PerfectWorld]
solveAll pw n
  | null updated = [pw]
  | otherwise = [pw'' | pw' <- updated, pw'' <- solveAll pw' (n+1)]
    where updated = updateWorld pw n

updateWorld pw n = concatMap (updateTower n) $
                   zip ((init . inits) pw) ((init . tails) pw)
updateTower n (h,t)
  | updateAble = [h ++ ((n:tower):tail t)]
  | otherwise = []
    where tower = head t
          updateAble = null tower ||
                       isPowerOf2 (n + head tower)


------------------------------------------------------------
-- slight better performance than the first version
-- but still it sucks sooo much...

type NumberOfEmptyTowers = Int
type PerfectWorld' = ([Tower],NumberOfEmptyTowers)

solve' n = solveAll' [([],n)] 1

solveAll' :: [PerfectWorld'] -> Int -> [PerfectWorld']
solveAll' pwl n
  | null updated = pwl
  | otherwise = solveAll' updated (n+1)
    where updated = concatMap (updateWorld' n) pwl

-- consider only left most empty tower
updateWorld' n (towers,k)
  | k <= 0 = updated
  | otherwise = ([n]:towers,k-1):updated
    where
      updated = map f $ concatMap (updateTower' n) $
                zip ((init . inits) towers) ((init . tails) towers)
      f ts = (ts,k)

updateTower' n (h,t)
  | updateAble = [h ++ ((n:tower):tail t)]
  | otherwise = []
    where tower = head t
          updateAble = isPowerOf2 (n + head tower)


----------------------------------------------------------

maxNumber n = (f n) + n - 1
  where f n = fromJust $ find (>= n) powerof2

-- TODO: MonadPlus Version

-- TODO: ST Monad Version
