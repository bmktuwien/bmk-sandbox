{-
author: Kim Bong Min
email:  kim-bong.min@rise-world.com
-}

import Data.List

--Problem 21.

insertAt :: Integral i => a -> [a] -> i -> [a]
insertAt e l i = p1 ++ (e:p2)
  where (p1,p2) = genericSplitAt (i-1) l

--Problem 22.
range :: Integral i => i -> i -> [i]
range a b = [a..b]
