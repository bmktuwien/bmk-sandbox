{-
Module      :  Sudoku Solver
Description :  very simple sudoku sovlver, which
               reads an incomplete sudoku from stdin and prints
               out all the found solutions.

input format:  1 2 3 0
               4 1 0 3
               3 4 1 2
               2 0 0 1

Maintainer  :  kim-bong.min@rise-world.com
-}

{-
*) how should I handle exceptions, i.e EOF? What's the best practise?
*) for practising purpose I didn't use some libraries, where I could've
   used one, like (lines and words)
*) how to write polymorphic (is it the correct term?) functions?
   I tried to write polymorphic functions like getElement, but I think
   I didn't quite grasp the concept yet...
*) how to avoid interleaved case expressions?
*) I am still little bit struggling with monads and functors...
*) My haskell coding style is terrible, in fact I don't have a style, cause
   I write the functions down, as I need them, which is obviously bad...
*) I am using emacs haskell mode, but as you can see the emacs indentation
   doesn't work sometimes. How should I configure it?
-}

import Data.List
import Data.Char
import Data.Maybe
import Data.Ord
import Control.Monad
import System.IO


main = do
  putStrLn "Greetings!"
  putStrLn "What dimension has your Sudoku? Type in a number:"
  inp <- getLine
  let n = read inp :: Int
  sudoku <- getNxNSudoku n
  case sudoku of
    Nothing  -> putStrLn "Sorry, your input was incorrect"
    Just sdk -> do putStrLn "Solving, one moment please..."
                   let result = solve sdk
                   case result of
                     Nothing -> putStrLn "Sorry, I couldn't solve your Sudoku"
                     Just resultList -> do mapM_ sudokuShow resultList
                                           putDelimeter 10
                                           putStrLn "Live long and prosper!"

--------------------------------------------------------------------------------
-- despair attempt to write own io functions

getWord :: IO String
getWord = do
  eof <- isEOF
  if eof then
    return []
    else do c <- getChar
            if isSpace c then
              return []
              else do tail <- getWord
                      return (c:tail)

getElement :: Read a => IO (Maybe a)
getElement = do
  input <- getWord
  case reads input of
    [(n,[])] -> return $ Just n
    _        -> return Nothing

getElements :: Read a => IO [Maybe a]
getElements = do
  eof <- isEOF
  if eof
     then return []
     else do elem <- getElement
             tail <- getElements
             return $ elem : tail

getList :: Read a => Int -> IO [Maybe a]
getList n = replicateM n getElement

getMatrix :: Read a => Int -> Int -> IO [[Maybe a]]
getMatrix n m = replicateM n (getList m)

putDelimeter :: Int -> IO()
putDelimeter n = putStrLn $ replicate n '='

--------------------------------------------------------------------------------
-- very simple backtracking sudoku solver

type Number = Integer
type Row = [Number]
type Column = [Number]
type Sudoku = [Row]
type Cell = (Int,Int)

getRow :: Sudoku -> Int -> Row
getRow s i = s !! i

getColumn :: Sudoku -> Int -> Column
getColumn s i = map (!! i) s

getCell :: Sudoku -> Cell -> Number
getCell sudoku (i,j) = (sudoku !! i) !! j

isValidSudoku :: Sudoku -> Bool
isValidSudoku sudoku =
  and [all ((== n)  . length) sudoku,
       all uniqueNumbers sudoku,
       all uniqueNumbers $ map (getColumn sudoku) [0..n-1]]
  where
    n = length sudoku
    uniqueNumbers r = null $ filter (/= 0) r \\ [1..toInteger n]


getEmptyCells :: Sudoku -> [Cell]
getEmptyCells s =
  [(i,j) | i <- [0..n-1], j <- [0..n-1], getCell s (i,j) == 0]
  where n = length s

getCandidatesForEmptyCell :: Sudoku -> Cell -> [Number]
getCandidatesForEmptyCell s (i,j) =
  [1..toInteger n] \\ (getRow s i `union` getColumn s j)
  where n = length s

-- omg, it's ugly, it was late, don't punch me :(
setCell :: Sudoku -> Cell -> Number -> Sudoku
setCell s (i,j) e =
  [row | x <- [0..n-1], let row = tmp x]
  where n = length s
        tmp x
          | x /= i = _row
          | otherwise = take j _row ++ (e:drop (j+1) _row)
          where
            _row = getRow s x

solve_ :: Sudoku -> [Sudoku]
solve_ sdk
  | null emptyCells = [sdk]
  | null candidates = []
  | otherwise = [s | cand <- candidates, s <- solve_ $ setCell sdk cell cand]
    where emptyCells = getEmptyCells sdk
          cl = [(ec,l) | ec <- emptyCells, let l = getCandidatesForEmptyCell sdk ec]
          (cell,candidates) =
            minimumBy (\(_,cl1) (_,cl2) -> comparing length cl1 cl2) cl

solve :: Sudoku -> Maybe [Sudoku]
solve sdk
  | not valid = Nothing
  | null result = Nothing
  | otherwise = Just result
  where valid = isValidSudoku sdk
        result = solve_ sdk


sudokuShow :: Sudoku -> IO ()
sudokuShow sudoku = do putDelimeter (2*n)
                       mapM_ showRow sudoku
  where showRow = putStrLn . foldr (\x a -> show x ++ (' ' : a)) []
        n = length sudoku

getNxNSudoku :: Int -> IO (Maybe Sudoku)
getNxNSudoku n = do
  input <- getMatrix n n :: IO [[Maybe Integer]]
  if any (elem Nothing) input
    then return Nothing
    else return $ Just $ map (map fromJust) input

