
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe

askNumber :: MaybeT IO Int
askNumber = do
  lift $ putStrLn "please insert a number"
  ns <- lift getLine
  let n = read ns :: Int
  guard (n >= 0 && n < 9)
  return n

guessNumber :: MaybeT IO ()
guessNumber = do
  n <- askNumber
  lift $ putStrLn $ "You entered " ++ (show n) ++ "! Didn' t you?"

data MyState = A | B | C | D
             deriving Eq

changeState 1 = A
changeState 2 = B
changeState 3 = C
changeState 4 = D
changeState _ = A

myTest :: State (MyState,Int) Int
myTest = do
  (state,number) <- get
  if (state == D)
    then return number
    else do put (changeState number, number+1)
            myTest


main :: IO ()
main = do
  print $ evalState myTest (A, 0)
