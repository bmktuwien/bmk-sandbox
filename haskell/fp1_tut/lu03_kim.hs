{-
Assigment 3
author: Kim Bong Min
e-mail: kim-bong.min@rise-world.com
date  : 2011.10.26
-}

import Data.List

-- 1

type Matrix = [[Integer]]

anp1 :: [[Integer]] -> Matrix
anp1 [] = [[1]]
anp1 l = map (take max . (++ repeat 0)) l
  where max = maximum $ map length l

-- 2

type Zeilen    = Integer
type Spalten   = Integer
type Fuellwert = Integer

anp2 :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
anp2 l z s w
  | z <= 0 || s <= 0 = error "unzulaessig"
  | otherwise = map (genericTake s . (++ repeat w)) $
                genericTake z $ l ++ repeat []

-- 3

transp :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
transp l z s w
  | z <= 0 || s <= 0 = error "unzulaessig"
  | otherwise = transpose $ anp2 l z s w

-- 4

type Laenge = Integer

sp :: [[Integer]] -> [[Integer]] -> Laenge -> Fuellwert -> Integer
sp l1 l2 len w
  | len <= 0 = error "unzulaessig"
  | otherwise = sum $ zipWith (*) zvec svec
    where zvec = concat $ anp2 l1 1 len w
          svec = concat $ anp2 l2 len 1 w

-- EOF --
