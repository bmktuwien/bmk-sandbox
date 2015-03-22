-- TODO: choose appropriate function names

import Data.List
import Data.Maybe

hilbert f r = magic l x (r-1)
  where
    x = startElement f
    l = helper x

magic l x 0 = x
magic l x 1 = l^2 - x
magic l x r
  | even q = l^2 + (q*l) + (f q) - x
  | otherwise =  ((q+1) * l) + (f q) + x
    where
      q   = r - 1
      f n = (n * (n+1)) `div` 2

startElement 1 = 1
startElement f = (f^2) `div` 2

helper n
  | (p^2) <= 2*n = (p+1)
  | otherwise = p
      where
        p = ceiling $ sqrt (fromIntegral (2 * n))

-- Expermient Box
---------------------------------------------------------
---------------------------------------------------------

squares :: [Integer]
squares = [i^2 | i <- [1..]]

{--
p f r = o f !! (r-1)
--}

p f r = p' start e (r-1)
  where
    start = startElement f
    helper n
      | (p^2) <= 2*n = (p+1)
      | otherwise = p
        where
          p = ceiling $ sqrt (fromIntegral (2 * n))
    e = helper start

p' acc e counter
   | counter == 0 = acc
   | otherwise = p' (e^2 - acc) (e+1) (counter - 1)

o f = g
  where
    start = startElement f
    helper n
      | (p^2) <= 2*n = (p+1)
      | otherwise = p
        where
          p = ceiling $ sqrt (fromIntegral (2 * n))
    l = [i^2 | i <- [helper start..]]
    g = start : rest
      where
        rest = zipWith (-) l g

-- naive (without any intelligent thoughts...)

starts = 1 : filter (not . test) [2..]

test n = any (>= n) $ map (findPartner . flip (-) n) l
  where
    start = ceiling $ sqrt (fromIntegral n)
    l = takeWhile (< (2 * n)) [i^2 | i <- [start..]]

findPartner n = helper2 start
   where
     start = ceiling $ sqrt (fromIntegral n)
     helper2 s
       | (s^2) - n <= n = helper2 (s+1)
       | otherwise = s^2 - n

---------------------------------------------------------
---------------------------------------------------------

-- little bit better
starts' = 1 : filter (not . test') [2,4..]

test' n
 | partner <= 0 = False
 | otherwise = findPartner' partner == n
    where
      p = floor $ sqrt (fromIntegral (2 * n))
      partner = (p ^ 2) - n

findPartner' n
 | x^2 == 2*n = (x+1)^2 - n
 | otherwise = (x^2) - n
   where
     x = ceiling $ sqrt (fromIntegral (2 * n))

---------------------------------------------------------
---------------------------------------------------------

-- OMG, i was so blind
starts'' = 1 : map (`div` 2) (tail squares)

---------------------------------------------------------

factors n = [(f,r) | f <- [1..n], n `rem` f == 0, let r = n `div` f]

-- copied from Haskell Wiki
isPrime n = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes

primes = 2 : filter isPrime [3,5..]

primeFactors n | n > 1 = go n primes
   where
     go n ps@(p:ps')
        | p*p > n        = [n]
        | n `rem` p == 0 =  p : go (n `quot` p) ps
        | otherwise      =      go n ps'

primeFactors' :: Integer -> [(Integer,Int)]
primeFactors' n =
    zip nubbed (map (\x -> length $ filter (== x) pList) nubbed)
    where pList = primeFactors n
          nubbed = nub pList

frs = [(f,r) | i <- [0..27], j <- [0..12], let f = 2^i * 3^j, let r = 71328803586048 `div` f]

result = sum $ map (uncurry hilbert) frs

-- EOF --
