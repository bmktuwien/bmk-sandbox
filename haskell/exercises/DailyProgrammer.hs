-- Challenge #122 [Easy]

intToDigits :: Integral a => a -> [Int]
intToDigits n
  | n < 0  = error "No negative numbers allowed"
  | n < 10 = [d]
  | otherwise = d : intToDigits (n `div` 10)
  where
    d = fromIntegral n `mod` 10

challenge122 :: Integral a => a -> Int
challenge122 n
  | s < 10    = s
  | otherwise = challenge122 s
  where
    s = sum $ intToDigits n

-- Challenge #121 [Easy]

bytelandianExchange :: [Integer] -> Int -> Int
bytelandianExchange []    acc = acc
bytelandianExchange coins acc =
  bytelandianExchange reduced (acc + reducedCounter)
  where
    f 0    = [0]
    f coin = [floor (c/2), floor (c/3), floor (c/4)]
      where
        c = fromInteger coin

    expanded = concatMap f coins

    reduced = filter (/= 0) expanded

    reducedCounter = length $ filter (== 0) expanded
