{-
Assigment 2
author: Kim Bong Min
e-mail: kim-bong.min@rise-world.com
date  : 2011.10.19
-}

import Data.Maybe
import Data.List

-- 1
p = [1 + 4*n | n <- [0..]]

istP n
  | n < 1 = False
  | otherwise = (n - 1) `mod` 4 == 0

istPrimal :: Integer -> Bool
istPrimal n
  | (n <= 1) || not (istP n) = False
  | otherwise = null $ faktorisiere n

-- 2
-- I know, it's very very dumb, but for the time being...
faktorisiere :: Integer -> [(Integer,Integer)]
faktorisiere n
  | not (istP n) = error "Unzulaessig"
  | otherwise = [(f1,f2) | f1 <- ls, f2 <- ls, f1 * f2 == n]
    where ls = takeWhile (< n) $ tail p

-- 3
type Editor = String
type Suchzeichenreihe = String
type Index = Integer
type Vorkommen = Integer
type Alt = String
type Neu = String

suche :: Editor -> Suchzeichenreihe -> Index
suche t p = toInteger $ fromMaybe (-1) $ findIndex (p `isPrefixOf`) (tails t)

-- 4
sucheAlle :: Editor -> Suchzeichenreihe -> [Index]
sucheAlle t p = map toInteger $ findIndices (p `isPrefixOf`) (tails t)


-- 5
-- TODO (find better solution)
ersetze :: Editor -> Vorkommen -> Alt -> Neu -> Editor
ersetze text i alt neu
  | i <= 0 = text
  | genericLength occs < i = text
  | otherwise = part1 ++ neu ++ drop (length alt) part2
    where
      occs = sucheAlle text alt
      ind  = occs `genericIndex` (i-1)
      (part1,part2) = genericSplitAt ind text

-- EOF --
