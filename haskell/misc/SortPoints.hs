import Data.List

data Point = Point { x :: Int
                   , y :: Int
                   }
             deriving (Show, Eq, Ord)

sortCountClockwise :: [Point] -> [Point]
sortCountClockwise ps = sortCountClockwise' ps'
  where
    ps' = sort ps

    sortCountClockwise' [] = []
    sortCountClockwise' [p] = [p]
    sortCountClockwise' (p1:ps)
      | y p1 >= y p2 = p1:p2:ps'
      | otherwise = p1:ps' ++ [p2]
      where
        (p2:ps') = sortCountClockwise' ps
