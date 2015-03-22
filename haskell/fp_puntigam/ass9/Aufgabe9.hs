data MaybeState s v = T (s -> StMaybe s v)

data StMaybe state value = Error   state
                         | Success (value,state)

instance Monad (MaybeState state) where
  return v = T (\s -> Success (v,s))

  T f >>= g = T (\s -> case f s of
                            Error   s'      -> Error s'
                            Success (v,s')  -> fun s'
                              where T fun = g v)


-- simple arithmetic interpreter supporting performance analysis

data Operation = None | Additive | Multiplicative
               deriving Eq

type NumberOfAddOp     = Int
type NumberOfMulOp     = Int
type NumberOfSuccAddOp = Int
type NumberOfSuccMulOp = Int

type State = (Operation,NumberOfAddOp,NumberOfMulOp,NumberOfSuccAddOp,NumberOfSuccMulOp)

incrAddOp     (_,a,m,sa,sm) = (Additive,a+1,m,sa,sm)
incrMulOp     (_,a,m,sa,sm) = (Multiplicative,a,m+1,sa,sm)
incrSuccAddOp (_,a,m,sa,sm) = (Additive,a,m,sa+1,sm)
incrSuccMulOp (_,a,m,sa,sm) = (Multiplicative,a,m,sa,sm+1)

stateUpdate :: Operation -> State -> State
stateUpdate Additive s@(o,_,_,_,_) =
  if o == Additive then incrSuccAddOp s else incrAddOp s
stateUpdate Multiplicative s@(o,_,_,_,_) =
  if o == Multiplicative then incrSuccMulOp s else incrMulOp s

execOp :: (Integer -> Integer -> Integer) -> Operation ->
          Integer -> Integer -> MaybeState State Integer
execOp binOp op a b
  | result  < 0 = T Error
  | otherwise   = T(\s -> Success (result,stateUpdate op s))
    where result = a `binOp` b

plus :: Integer -> Integer -> MaybeState State Integer
plus = execOp (+) Additive

minus :: Integer -> Integer -> MaybeState State Integer
minus = execOp (-) Additive

mal :: Integer -> Integer -> MaybeState State Integer
mal = execOp (*) Multiplicative

durch :: Integer -> Integer -> MaybeState State Integer
durch = execOp div Multiplicative

rest :: Integer -> Integer -> MaybeState State Integer
rest = execOp rem Multiplicative


-- testing

prog1 = do x <- plus 1 2
           y <- mal x 3
           mal y 2

prog2 = do x <- plus 1 2
           y <- minus x 4
           plus y 10

test :: MaybeState State Integer -> (Integer,Int,Int,Int,Int)
test (T p) =
  case result of
       Error (_,a,m,sa,sm)       -> (-1,a,m,sa,sm)
       Success (v,(_,a,m,sa,sm)) -> (v,a,m,sa,sm)
    where result = p (None,0,0,0,0)


-- EOF --
