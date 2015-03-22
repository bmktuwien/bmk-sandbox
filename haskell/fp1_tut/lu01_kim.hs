{-
Assigment 1
author: Kim Bong Min
e-mail: kim-bong.min@rise-world.com
date  : 2011.10.19
-}

import Data.List

-- 1
pick :: Integer -> [Integer] -> [Integer]
pick n = filter (== n)

-- 2
pickAll :: [Integer] -> [Integer] -> [Integer]
pickAll ls = filter (`elem` ls)

-- 3
variations :: Integer -> Integer -> Integer
variations n r
  | n >= r && r >= 0 = product [n-r+1..n]
  | otherwise = -1

-- 4
type Symbol   = Char
type Text     = String
type NumberOf = Integer

numberOfOcc :: Symbol -> Text -> NumberOf
numberOfOcc c = toInteger . length . filter (c ==)

-- 5
mostCommonSymbol :: Text -> Symbol
mostCommonSymbol [] = error "kein Resultat"
mostCommonSymbol text
  | 1 == maxs  = c
  | otherwise  = error "kein Resultat"
    where
      groups  = map (\x -> (numberOfOcc x text, x)) $ nub text
      (max,c) = maximum groups
      maxs    = length $ filter ((== max) . fst) groups
