{-
Assigment 6
author: Kim Bong Min
e-mail: kim-bong.min@rise-world.com
date  : 2011.11.24
-}

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random
import Text.Groom

data DOrd = Infix | Praefix | Postfix |
            GInfix | GPraefix | GPostfix

data BTree = Nil | BNode Int BTree BTree
             deriving (Show,Eq,Ord)

-- 1

flatten :: BTree -> DOrd -> [Int]
flatten Nil _ = []
flatten (BNode i chl chr) Infix = flatten chl Infix ++ (i : flatten chr Infix)
flatten (BNode i chl chr) Praefix = (i : flatten chl Praefix) ++ flatten chr Praefix
flatten (BNode i chl chr) Postfix = flatten chl Postfix ++ flatten chr Postfix ++ [i]
flatten bt GInfix = reverse $ flatten bt Infix
flatten bt GPraefix = reverse $ flatten bt Praefix
flatten bt GPostfix = reverse $ flatten bt Postfix


-- 2

isST :: BTree -> Bool
isST Nil = True
isST (BNode i chl chr) = and [p1,p2,isST chl, isST chr]
    where
      p1 = all (i > ) $ flatten chl Infix
      p2 = all (i < ) $ flatten chr Infix

-- 3

type Control = String
type Data    = Integer
--type Func    = Data -> Data
data Tree    = Leaf Func
             | Node Func Tree Tree Tree
               deriving Show

mkControl :: String -> Control
mkControl = filter (`elem` "lmr")

apply :: Control -> Data -> Tree -> Data
apply _  d (Leaf (Func { func = f })) = f d
apply [] d (Node (Func { func = f }) _ _ _) = f d
apply control d (Node (Func { func = f }) tl tm tr) =
    case c of
      'l' -> apply cs (f d) tl
      'm' -> apply cs (f d) tm
      'r' -> apply cs (f d) tr
    where
      (c:cs) = mkControl control -- HOW DUMB!!! :)

-- 4

data LTree = LNode Data [LTree]
    deriving Show

mapLT :: Func -> LTree -> LTree
mapLT fu@(Func { func = f }) (LNode i chlist) = LNode (f i) (map (mapLT fu) chlist)

------------------------------------------------
------------ END OF ASSIGNMENT CODE ------------
------------------------------------------------

-- random data generators

instance Arbitrary BTree where
    arbitrary = sized arbitrary'
        where
          arbitrary' s
              | s <= 0 = return Nil
              | s > 0  = oneof [return Nil, liftM3 BNode (choose (1,100)) subtree subtree]
              where
                subtree = arbitrary' (s - 1)

randomBTrees = unGen (vectorOf 15 (resize 30 arbitrary)) (System.Random.mkStdGen 2) 1 :: [BTree]

printRandomBTreeTestCases = do
  let f t = putStrLn $ "flatten (" ++ show t ++ ") Infix" ++ " == " ++
            show (flatten t Infix)
      g t = putStrLn $ "flatten (" ++ show t ++ ") Praefix" ++ " == " ++
            show (flatten t Praefix)
      h t = putStrLn $ "flatten (" ++ show t ++ ") Postfix" ++ " == " ++
            show (flatten t Postfix)

  mapM_ f $ filter (/= Nil) randomBTrees
  mapM_ g $ filter (/= Nil) randomBTrees
  mapM_ h $ filter (/= Nil) randomBTrees


----------------------------------------------------

randomSTrees = map mkSTree [[1..n] | n <- [10,20,30,40,50,60]]

mkSTree [] = Nil
mkSTree ls = BNode i (mkSTree l) (mkSTree (tail r))
    where
      m = length ls `div` 2
      i = ls !! m
      (l,r) = splitAt m ls

printRandomSTreeTestCases = do
  let f t = putStrLn $ "isST (" ++ show t ++ ") " ++ " == " ++
            show (isST t)

  mapM_ f $ filter (/= Nil) randomBTrees
  mapM_ f randomSTrees

-------------------------------------------------------

data Op = Add | Mult

data Func = Func { op :: Op
                 , operand1 :: Integer
                 , func :: Integer -> Integer
                 }

instance Show Func where
    show (Func Add n _) = "(+ " ++ show n ++ ")"
    show (Func Mult n _) = "(* " ++ show n ++ ")"

instance Arbitrary Func where
    arbitrary = do
      n <- choose (1,100) :: Gen Integer
      oneof [ return (Func Add n (+ n))
            , return (Func Mult n (* n))
            ]

instance Arbitrary Tree where
    arbitrary = sized arbitrary'
        where arbitrary' s
                  | s <= 0 = liftM Leaf arbitrary
                  | otherwise = oneof [ liftM Leaf arbitrary
                                      , liftM4 Node arbitrary subtree subtree subtree
                                      ]
                  where subtree = arbitrary' (s `div` 2)

randomTrees = unGen (vectorOf 15 (resize 60 arbitrary)) (System.Random.mkStdGen 17) 1 :: [Tree]

printRandomTreeTestCases = do
  let gen = System.Random.mkStdGen 7
      control = vectorOf 5 (oneof [return 'l', return 'm', return 'r'])
      f (control,n,t) = putStrLn $ "apply \"" ++ control ++ "\" " ++ show n ++ " (" ++ show t ++ ") "
                        ++ " == " ++ show (apply control n t)

  mapM_ f $ zip3 (unGen (vectorOf (length randomTrees) control) gen 1)
                 (unGen (vectorOf (length randomTrees) (choose (1,100))) gen 1)
                 randomTrees

-------------------------------------------------------

instance Arbitrary LTree where
    arbitrary = sized arbitrary'
        where arbitrary' s
                  | s <= 0 = liftM2 LNode (choose (1,100)) (return [])
                  | otherwise = oneof [ liftM2 LNode (choose (1,100)) (sequence [subtree])
                                      , liftM2 LNode (choose (1,100)) (sequence [subtree,subtree,subtree])
                                      ]
                  where
                    subtree = arbitrary' (s `div` 3)

randomLTrees = unGen (vectorOf 15 (resize 60 arbitrary)) (System.Random.mkStdGen 20) 1 :: [LTree]

printRandomLTreeTestCases = do
  let gen = System.Random.mkStdGen 13
      f func t = putStrLn $ "mapLT " ++ show func ++ " (" ++ show t ++ ") "
                 ++ " == " ++ show (mapLT func t)

  mapM_ (uncurry f) (zip (unGen (vectorOf (length randomLTrees) arbitrary) gen 1) randomLTrees)


--------------------------------------------------------

main = do
  putStrLn "-- random testcases for `flatten`"
  putStrLn ""
  printRandomBTreeTestCases
  putStrLn ""
  putStrLn ""
  putStrLn "-- random testcases for `isST`"
  putStrLn ""
  printRandomSTreeTestCases
  putStrLn ""
  putStrLn ""
  putStrLn "-- random testcases for `apply`"
  putStrLn ""
  printRandomTreeTestCases
  putStrLn ""
  putStrLn ""
  putStrLn "-- random testcases for `mapLT`"
  putStrLn ""
  printRandomLTreeTestCases
  putStrLn ""
  putStrLn ""
