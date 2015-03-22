{-
Assigment 5
author: Kim Bong Min
e-mail: kim-bong.min@rise-world.com
date  : 2011.11.09
-}

unixtac :: String -> String
unixtac = unlines . reverse . lines

unixhead :: Int -> String -> String
unixhead n = unlines . (take n) . lines

unixtail :: Int -> String -> String
unixtail n = unlines . reverse . (take n) . reverse . lines

aslines :: ([String]->[String]) -> String -> String
aslines f = unlines . f . lines

unixtac' = aslines reverse
unixhead' n = aslines (take n)
unixtail' n = aslines (reverse . (take n) . reverse)

unixrev :: String -> String
unixrev = aslines (map reverse)

wordrev :: String -> String
wordrev = aslines (map (unwords . reverse . words))

unixwcw :: String -> Int
unixwcw = length . (concatMap words) . lines

unixwc :: String -> (Int,Int,Int)
unixwc s = (l1,l2,l3)
  where l1 = length $ lines s
        l2 = length $ concatMap words $ lines s
        l3 = length s

-- EOF --
