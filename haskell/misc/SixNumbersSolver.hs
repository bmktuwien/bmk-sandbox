{-# LANGUAGE BangPatterns #-}

import Data.Maybe
import Data.List
import Criterion.Main

solveSixNumbers :: [Int] -> Int -> Maybe [String]
solveSixNumbers numbers target = solveSixNumbers' numbers []
  where
    solveSixNumbers' [] _  = Nothing
    solveSixNumbers' numbers res
      | elem target numbers = Just . reverse $ (show target) : res
      | otherwise = listToMaybe . catMaybes $
                    [ solveSixNumbers' (reduce numbers n1 n2 n3)
                      (mkResultStr n1 n2 n3 opStr : res) |
                      opStr <- ["+", "-", "*", "/"],
                      (n1,n2) <- variationPairs numbers,
                      let op = binOp opStr,
                      let n3 = op n1 n2,
                      n3 > 0,
                      opStr /= "/" || n2 > 0 && n1 `mod` n2 == 0 ]

    binOp :: String -> Int -> Int -> Int
    binOp "+" = (+)
    binOp "-" = (-)
    binOp "*" = (*)
    binOp "/" = quot

    reduce ns n1 n2 n3 = n3 : (ns \\ [n1, n2])

    mkResultStr n1 n2 n3 opStr = show n1 ++ " " ++ opStr ++ " " ++ show n2 ++
                                 " = " ++ show n3

--------------------------------------------------------------------------------
data Op = Add | Sub | Mul | Div
        deriving Show

solveSixNumbers2 :: [Int] -> Int -> Maybe [String]
solveSixNumbers2 numbers target = solveSixNumbers' numbers []
  where
    solveSixNumbers' [] _  = Nothing
    solveSixNumbers' numbers res
      | elem target numbers = Just . reverse $ (show target) : res
      | otherwise = listToMaybe . catMaybes $
                    [ solveSixNumbers' (reduce numbers n1 n2 n3)
                      (mkResultStr n1 n2 n3 opType : res) |
                      (opType, op) <- [(Add, (+)),
                                       (Sub, (-)),
                                       (Mul, (*)),
                                       (Div, quot)],
                      let pairs | Add <- opType = combinationPairs numbers
                                | Mul <- opType = combinationPairs numbers
                                | otherwise = variationPairs numbers,
                      (n1,n2) <- pairs,
                      guard opType n1 n2,
                      let n3 = op n1 n2,
                      n3 > 0 ]

    guard Div n1 n2 = n2 > 0 && n1 `mod` n2 == 0
    guard _ _ _     = True

    reduce ns n1 n2 n3 = n3 : (ns \\ [n1, n2])

    mkResultStr n1 n2 n3 opType = show n1 ++ " " ++ show opType ++ " " ++ show n2 ++
                                  " = " ++ show n3

variationPairs []     = []
variationPairs (x:xs) = concatMap (\y -> [(x,y),(y,x)]) xs ++ variationPairs xs

combinationPairs []     = []
combinationPairs (x:xs) = map ((,) x) xs ++ combinationPairs xs

main :: IO ()
main = defaultMain [
  bench "worst case 6 numbers" (nf (solveSixNumbers2 [1,2,3,4,5,6]) 0)
  ]

-- main :: IO ()
-- main = print $ solveSixNumbers [1,2,3,4,5,6] 0
