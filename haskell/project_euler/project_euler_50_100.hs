import Data.Numbers.Primes
import Data.Digits
import Data.List
import qualified Data.HashMap as HM

-- Utils ------------------------------------------

allTheSame :: Eq a => [a] -> Bool
--allTheSame [] = True
allTheSame xs = and $ map (== head xs) (tail xs)

powerset [] = [[]]
powerset (x:xs) = [(x:ys) | ys <- v] ++ v
  where
    v = powerset xs

extractIndices idxs ls = extractIndices' 1 idxs ls
  where
    extractIndices' _ _ [] = ([],[])
    extractIndices' _ [] ls = ([],ls)
    extractIndices' t (i:is) (x:xs)
      | t == i = (x:es, rs)
      | otherwise = (es', x:rs')
      where
        (es,rs) = extractIndices' (t+1) is xs
        (es',rs') = extractIndices' (t+1) (i:is) xs

-- 51 ---------------------------------------------

solve51 :: [[Int]]
solve51 = filter (\x -> length x == 8) $
          map snd $ concatMap (\(_,m) -> HM.toList m) $ HM.toList $ solve51' 5
  where
    solve51' :: Int -> HM.Map Int (HM.Map Int [Int])
    solve51' x = foldl' (\m t -> buildSearchMap t m) HM.empty $ concatMap candidates searchSpace
      where
        searchSpace = takeWhile (< 10^(x+1)) $ dropWhile (< 10^x) primes

        candidates p = map (\(ixs,(es,rs)) -> (unDigits 10 ixs, p, unDigits 10 rs)) $
                       filter (\(ixs,(es,rs)) -> allTheSame es) $
                       map (\idxs -> (idxs,extractIndices idxs $ digits 10 p)) $
                       filter (\l -> not (null l) && length l /= x) $
                       powerset [1..x]

        buildSearchMap (k,n,m) sMap = HM.insert k sMap'' sMap
          where
            sMap' = HM.findWithDefault HM.empty k sMap
            sMap'' = HM.insertWith (++) m [n] sMap'
