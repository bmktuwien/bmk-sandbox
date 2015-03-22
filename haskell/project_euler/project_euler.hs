import Data.Numbers.Primes
import Control.Monad
import Control.Monad.Logic
import Data.Maybe
import Data.List
import Data.Ord
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Groom

-- 2

fib = 1 : 1 : (zipWith (+) fib (tail fib))

solve2 = sum $ filter (even) $ takeWhile (<= 4000000) fib

-- 3

solve3 = maximum $ primeFactors 600851475143

-- 4

isPalindrom :: Int -> Bool
isPalindrom n = s == reverse s
  where s = show n

solve4 = (find isPalindrom . reverse . sort) $ [x*y | x <- [999,998..100], y <- [999,998..100]]

-- 5

divisibleBy n x = n `rem` x == 0
isDivisibleByAll n list = all (n `divisibleBy`) list

reduce n list =
  case found of
    Nothing -> n
    Just n' -> reduce n' list
  where cand = map (n `quot`) $ filter (n `divisibleBy`) list
        found = find (`isDivisibleByAll` list) cand

solve5 = reduce (product [2..20]) [2..20]

-- 6

solve6 = n2 - n1
  where
    n1 = sum $ [i^2 | i <- [1..100]]
    n2 = (sum [1..100])^2

-- 7

solve7 = primes !! 10000

-- 8

inp = map digitToInt $
      "73167176531330624919225119674426574742355349194934" ++
      "96983520312774506326239578318016984801869478851843" ++
      "85861560789112949495459501737958331952853208805511" ++
      "12540698747158523863050715693290963295227443043557" ++
      "66896648950445244523161731856403098711121722383113" ++
      "62229893423380308135336276614282806444486645238749" ++
      "30358907296290491560440772390713810515859307960866" ++
      "70172427121883998797908792274921901699720888093776" ++
      "65727333001053367881220235421809751254540594752243" ++
      "52584907711670556013604839586446706324415722155397" ++
      "53697817977846174064955149290862569321978468622482" ++
      "83972241375657056057490261407972968652414535100474" ++
      "82166370484403199890008895243450658541227588666881" ++
      "16427171479924442928230863465674813919123162824586" ++
      "17866458359124566529476545682848912883142607690042" ++
      "24219022671055626321111109370544217506941658960408" ++
      "07198403850962455444362981230987879927244284909188" ++
      "84580156166097919133875499200524063689912560717606" ++
      "05886116467109405077541002256983155200055935729725" ++
      "71636269561882670428252483600823257530420752963450"

solve8 = maximum $ map (product . (take 5)) $ tails inp

-- 9

solve9 = (product . head) [[a,b,c] | a <- [1..1000], b <- [a+1..999-a],
                           let c = 1000 - (a+b), a^2 + b^2 == c^2]

-- 10

solve10 = sum $ takeWhile (< 2000000) primes

-- 11

inp11 =
  [[08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08],
   [49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00],
   [81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65],
   [52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91],
   [22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80],
   [24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50],
   [32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70],
   [67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21],
   [24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72],
   [21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95],
   [78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92],
   [16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57],
   [86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58],
   [19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40],
   [04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66],
   [88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69],
   [04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36],
   [20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16],
   [20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54],
   [01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]]

horizIndices len p = [(i,j) | i <- [0..len-1], j <- [0..len-p]]
vertIndices len p = [(i,j) | j <- [0..len-1], i <- [0..len-p]]
diagIndices len p = [(i,j) | i <- [0..len-1], j <- [0..len-1], len - i >= p && len - j >= p]

getElem (i,j) m = (m !! i) !! j
getHorizontal p m (i,j) = [getElem (i,j+x) m | x <- [0..p-1]]
getVertical p m (i,j) = [getElem (i+x,j) m | x <- [0..p-1]]
getDiagonalD p m (i,j) = [getElem (i+x,j+x) m | x <- [0..p-1]]
getDiagonalU p m (i,j) = [getElem (i+x,j+(p-1-x)) m | x <- [0..p-1]]

solve11 = maximum $ map product $ map (getHorizontal 4 inp11) (horizIndices 20 4) ++
                                  map (getVertical 4 inp11) (vertIndices 20 4) ++
                                  map (getDiagonalD 4 inp11) (diagIndices 20 4) ++
                                  map (getDiagonalU 4 inp11) (diagIndices 20 4)

-- 12

numberOfFactors n =  product $ map (\x -> ((+1) . length) $ filter (== x)  pfs) (nub pfs)
  where pfs = primeFactors n

trianglenumbers = [sum [0..n] | n <- [2..]]

solve12 = solve12' 1 1
  where solve12' n i
          | numberOfFactors (n + i + 1) > 500 = n + i + 1
          | otherwise = solve12' (n+i+1) (i+1)

-- 13

inp13 = [
  37107287533902102798797998220837590246510135740250,
  46376937677490009712648124896970078050417018260538,
  74324986199524741059474233309513058123726617309629,
  91942213363574161572522430563301811072406154908250,
  23067588207539346171171980310421047513778063246676,
  89261670696623633820136378418383684178734361726757,
  28112879812849979408065481931592621691275889832738,
  44274228917432520321923589422876796487670272189318,
  47451445736001306439091167216856844588711603153276,
  70386486105843025439939619828917593665686757934951,
  62176457141856560629502157223196586755079324193331,
  64906352462741904929101432445813822663347944758178,
  92575867718337217661963751590579239728245598838407,
  58203565325359399008402633568948830189458628227828,
  80181199384826282014278194139940567587151170094390,
  35398664372827112653829987240784473053190104293586,
  86515506006295864861532075273371959191420517255829,
  71693888707715466499115593487603532921714970056938,
  54370070576826684624621495650076471787294438377604,
  53282654108756828443191190634694037855217779295145,
  36123272525000296071075082563815656710885258350721,
  45876576172410976447339110607218265236877223636045,
  17423706905851860660448207621209813287860733969412,
  81142660418086830619328460811191061556940512689692,
  51934325451728388641918047049293215058642563049483,
  62467221648435076201727918039944693004732956340691,
  15732444386908125794514089057706229429197107928209,
  55037687525678773091862540744969844508330393682126,
  18336384825330154686196124348767681297534375946515,
  80386287592878490201521685554828717201219257766954,
  78182833757993103614740356856449095527097864797581,
  16726320100436897842553539920931837441497806860984,
  48403098129077791799088218795327364475675590848030,
  87086987551392711854517078544161852424320693150332,
  59959406895756536782107074926966537676326235447210,
  69793950679652694742597709739166693763042633987085,
  41052684708299085211399427365734116182760315001271,
  65378607361501080857009149939512557028198746004375,
  35829035317434717326932123578154982629742552737307,
  94953759765105305946966067683156574377167401875275,
  88902802571733229619176668713819931811048770190271,
  25267680276078003013678680992525463401061632866526,
  36270218540497705585629946580636237993140746255962,
  24074486908231174977792365466257246923322810917141,
  91430288197103288597806669760892938638285025333403,
  34413065578016127815921815005561868836468420090470,
  23053081172816430487623791969842487255036638784583,
  11487696932154902810424020138335124462181441773470,
  63783299490636259666498587618221225225512486764533,
  67720186971698544312419572409913959008952310058822,
  95548255300263520781532296796249481641953868218774,
  76085327132285723110424803456124867697064507995236,
  37774242535411291684276865538926205024910326572967,
  23701913275725675285653248258265463092207058596522,
  29798860272258331913126375147341994889534765745501,
  18495701454879288984856827726077713721403798879715,
  38298203783031473527721580348144513491373226651381,
  34829543829199918180278916522431027392251122869539,
  40957953066405232632538044100059654939159879593635,
  29746152185502371307642255121183693803580388584903,
  41698116222072977186158236678424689157993532961922,
  62467957194401269043877107275048102390895523597457,
  23189706772547915061505504953922979530901129967519,
  86188088225875314529584099251203829009407770775672,
  11306739708304724483816533873502340845647058077308,
  82959174767140363198008187129011875491310547126581,
  97623331044818386269515456334926366572897563400500,
  42846280183517070527831839425882145521227251250327,
  55121603546981200581762165212827652751691296897789,
  32238195734329339946437501907836945765883352399886,
  75506164965184775180738168837861091527357929701337,
  62177842752192623401942399639168044983993173312731,
  32924185707147349566916674687634660915035914677504,
  99518671430235219628894890102423325116913619626622,
  73267460800591547471830798392868535206946944540724,
  76841822524674417161514036427982273348055556214818,
  97142617910342598647204516893989422179826088076852,
  87783646182799346313767754307809363333018982642090,
  10848802521674670883215120185883543223812876952786,
  71329612474782464538636993009049310363619763878039,
  62184073572399794223406235393808339651327408011116,
  66627891981488087797941876876144230030984490851411,
  60661826293682836764744779239180335110989069790714,
  85786944089552990653640447425576083659976645795096,
  66024396409905389607120198219976047599490197230297,
  64913982680032973156037120041377903785566085089252,
  16730939319872750275468906903707539413042652315011,
  94809377245048795150954100921645863754710598436791,
  78639167021187492431995700641917969777599028300699,
  15368713711936614952811305876380278410754449733078,
  40789923115535562561142322423255033685442488917353,
  44889911501440648020369068063960672322193204149535,
  41503128880339536053299340368006977710650566631954,
  81234880673210146739058568557934581403627822703280,
  82616570773948327592232845941706525094512325230608,
  22918802058777319719839450180888072429661980811197,
  77158542502016545090413245809786882778948721859617,
  72107838435069186155435662884062257473692284509516,
  20849603980134001723930671666823555245252804609722,
  53503534226472524250874054075591789781264330331690]

solve13 = sum inp13

-- 14

lengthOfChain' :: Int -> Int
lengthOfChain' s
  | s == 1 = 1
  | even s = 1 + lengthOfChain' (s `div` 2)
  | otherwise = 1 + lengthOfChain' (3*s + 1)

memoMap :: Map Int Int
memoMap = M.fromList [(k,lengthOfChain' k) | k <- [3,5..10001]]

-- fail ??
memoized_lengthOfChain :: Int -> Int
memoized_lengthOfChain s
  | s == 1 = 1
  | even s = 1 + memoized_lengthOfChain (s `div` 2)
  | otherwise = 1 + f s
    where f s' =
            case (M.lookup s memoMap) of
              Nothing -> memoized_lengthOfChain (3*s + 1)
              Just l  -> l

solve14' = do
  let m1 = maximum $ map memoized_lengthOfChain [2..100000]
  putStrLn  $ "phase1 done: " ++ show m1
  let m2 = maximum $ map memoized_lengthOfChain [100001..200000]
  putStrLn  $ "phase2 done: " ++ show m2
  let m3 = maximum $ map memoized_lengthOfChain [200001..300000]
  putStrLn  $ "phase3 done: " ++ show m3
  let m4 = maximum $ map memoized_lengthOfChain [300001..400000]
  putStrLn  $ "phase4 done: " ++ show m4
  let m5 = maximum $ map memoized_lengthOfChain [400001..500000]
  putStrLn  $ "phase5 done: " ++ show m5
  let m6 = maximum $ map memoized_lengthOfChain [500001..600000]
  putStrLn  $ "phase6 done: " ++ show m6
  let m7 = maximum $ map memoized_lengthOfChain [700001..800000]
  putStrLn  $ "phase7 done: " ++ show m7
  let m8 = maximum $ map memoized_lengthOfChain [800001..900000]
  putStrLn  $ "phase8 done: " ++ show m8
  let m9 = maximum $ map memoized_lengthOfChain [900001..999999]
  putStrLn  $ "phase9 done: " ++ show m9

solve14 = maximum $ map (\x -> (memoized_lengthOfChain x, x)) [2..999999]

-- 15

findRoute :: MonadPlus m => (Int,Int) -> Int -> m [(Int,Int)]
findRoute (x,y) len
  | x == len && y == len = return []
  | otherwise = do
      pos' <- msum . map return $ [(x+1,y),(x,y+1)]
      pos <- f pos' len
      tails <- findRoute pos len
      return (pos : tails)
    where
    f :: MonadPlus m => (Int,Int) -> Int -> m (Int,Int)
    f (p,q) len
      | p > len || q > len = mzero
      | otherwise = return $ (p,q)

findRoutes :: Int -> Int
findRoutes l = length . observeAll $ findRoute (0,0) l

-- Now without monad plus

-- damn, too slow
magic (0,_) = 1
magic (_,0) = 1
magic (i,j) = magic (i-1,j) + magic (i,j-1)


magic'' accl count len
  | count >= len = last accl
  | otherwise = magic'' (map sum $ (tail . inits) accl) (count+1) len

magic''' n = magic'' (replicate (n+1) 1) 1 (n+1)

solve15 = magic''' 20

-- 16

solve16 = sum $ map digitToInt (show (2^1000))

-- 17

toWord 0 = ""
toWord 1 = "one"
toWord 2 = "two"
toWord 3 = "three"
toWord 4 = "four"
toWord 5 = "five"
toWord 6 = "six"
toWord 7 = "seven"
toWord 8 = "eight"
toWord 9 =  "nine"
toWord 10 = "ten"
toWord 11 = "eleven"
toWord 12 = "twelve"
toWord 13 = "thirteen"
toWord 14 = "fourteen"
toWord 15 = "fifteen"
toWord 16 = "sixteen"
toWord 17 = "seventeen"
toWord 18 = "eighteen"
toWord 19 = "nineteen"
toWord 20 = "twenty"
toWord 30 = "thirty"
toWord 40 = "forty"
toWord 50 = "fifty"
toWord 60 = "sixty"
toWord 70 = "seventy"
toWord 80 = "eighty"
toWord 90 = "ninety"
toWord n
  | n > 20 && n < 100 = toWord (n - (n `rem` 10)) ++ toWord (n `rem` 10)
  | n < 1000 && n `rem` 100 == 0 = toWord (n `div` 100) ++ "hundred"
  | n > 100 && n < 1000 = toWord (n `div` 100) ++ "hundredand" ++
                          toWord (n `rem` 100)
  | otherwise = "onethousand"

solve17 = sum $ map (length . toWord) [1..1000]


-- 18

inp18 = [[75],
         [95,64],
         [17,47,82],
         [18,35,87,10],
         [20,04,82,47,65],
         [19,01,23,75,03,34],
         [88,02,77,73,07,63,67],
         [99,65,04,28,06,16,70,92],
         [41,41,26,56,83,40,80,70,33],
         [41,48,72,33,47,32,37,16,94,29],
         [53,71,44,65,25,43,91,52,97,51,14],
         [70,11,33,28,77,73,17,78,39,68,17,57],
         [91,71,52,38,17,14,91,43,58,50,27,29,48],
         [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
         [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

solve18' acc [] = acc
solve18' acc (l:ls) = solve18' acc' ls
  where t  = map (take 2) $ tails acc
        t' = take ((length t) - 2) t
        acc' = zipWith (+) l (map maximum t')

solve18'' i = solve18' (replicate ((length i) + 1) 0) (reverse i)

solve18 = solve18'' inp18

-- 19

isLeapYear y = (y `rem` 400 /= 0) &&
               (y `rem` 4 == 0)
daysInMonth y m
  | (m == 0) || (m == 2) ||
    (m == 4) || (m == 6) ||
    (m == 7) || (m == 9) ||
    (m == 11) = 31
  | (m == 3) ||
    (m == 5) || (m == 8) ||
    (m == 10) = 30
  | (m == 1) && (isLeapYear y) = 29
  | (m == 1) && (not $ isLeapYear y) = 28
  | otherwise = error "shouldn't be here"

calcSundays :: (Int,Int,Int) -> (Int,Int) -> Int
calcSundays (y,m,d) (yb,mb)
  | y >= yb && m >= mb = 0
  | d == 6 = 1 + calcSundays (y',nextM,nextD) (yb,mb)
  | otherwise = calcSundays (y',nextM,nextD) (yb,mb)
  where
    nextM = (m+1) `rem` 12
    nextD = (d + (daysInMonth y m)) `rem` 7
    y'
      | m == 11 = (y+1)
      | otherwise = y

solve19 = calcSundays (1901,0,1) (2000,11)

-- 20

solve20 = sum $ map digitToInt (show (product [1..100]))

-- 21

d n = sum facs
  where facs = init $ factors n

amicable n = (n /= n') && (n == n'')
  where n' = d n
        n'' = d n'

amicables = [x | x <- [2..], amicable x]

solve21 = sum $ takeWhile (< 10000) amicables

-- 22

readLine22 :: BL.ByteString -> [String]
readLine22 line = map (init . tail . BL.unpack) (BL.split ',' line)

scoreName (name,ind) = score * ind
  where score = sum $ map (\x -> 1 + (fromJust $ elemIndex x alphabets)) name
        alphabets = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

solve22 = do
  raw <- BL.readFile "names.txt"
  let inp22 =  concatMap readLine22 $ BL.lines raw
      list = zip (sort inp22) [1..(length inp22)]
  print . sum $ map scoreName list

-- 23

-- very inefficient --

abundant n = (sum facs) > n
  where facs = init $ factors n

abundants = [x | x <- [1..], abundant x]

allPairs [] = []
allPairs (x:xs) = map (\y -> [x,y]) xs ++ allPairs xs

abundants' = (takeWhile (<= (28123)) abundants)

helper2 _ [] = []
helper2 x (a:as)
  | x+a > 28123 = [] --pruning
  | otherwise = (x+a) : (helper2 x as)

helper [] = Set.empty
helper ass@(a:as) = l `Set.union` (helper as)
  where l = Set.fromList $ helper2 a ass

solve23 = ((x+1)*(x) `div` 2)  - (sum l)
  where l = (Set.toList . helper) abundants'
        x = last l

-- 24

sorted_permutations [] = [[]]
sorted_permutations l = [p:ps | p <- l, let l' = delete p l, ps <- sorted_permutations l']

solve24 = (sorted_permutations [0,1,2,3,4,5,6,7,8,9]) !! (10^6-1)

-- 25

countDigits n
  | n < 10 = 1
  | otherwise = 1 + (countDigits (n `div` 10))

solve25 = find (\x -> (countDigits . fst) x >= 1000) (zip fib [1..])

-- 26

periodLength p = fromJust $ find f [1..]
  where f x = 10^x `mod` p == 1

solve26 = maximumBy (comparing periodLength) $
          takeWhile (< 1000) $ drop 3 primes

--27

helper27 a b = helper27' a b 0
  where
    helper27' a b n
      | p = 1 + helper27' a b (n+1)
      | otherwise = 0
      where
        p = isPrime (n^2 + a*n + b)

test1 = maximum [(v,a,b) | a <- [(-1),(-2)..(-999)], b <- [abs(a)..999], let v = helper27 a b, v > 1]
test2 = maximum [(v,a,b) | a <- [1..999], b <- [1..999], let v = helper27 a b, v > 1]

solve27 = a*b
  where
    (_,a,b) = maximum [test1,test2]


-- 28

helper28 n x bound (diag1,diag2)
  | n > bound = (diag1,diag2)
  | otherwise = helper28 (n+2) x' bound (de1++diag1,de2++diag2)
  where
    de1 = [x+(n-1),x+3*(n-1)]
    de2 = [x+2*(n-1),x+4*(n-1)]
    x' = x+4*(n-1)

solve28 = (sum d1) + (sum d2) + 1
  where
    (d1,d2) = helper28 3 1 1001 ([],[])

-- 29

solve29 = length . nub $ [a^b | a <- [2..100], b <- [2..100]]

-- 30

getDigits n = map digitToInt (show n)

test30 n = n == s
  where s = sum $ map (\x -> x^5) $ getDigits n

solve30 = sum $ [x | x <- [2..999999], test30 x]

-- 31

helper31 = [ 1 |
            p1 <- [0..200],
            p2 <- [0..((200-p1)`div`2)],
            p5 <- [0..((200-2*p2-p1) `div` 5)],
            p10 <- [0..((200-p1-2*p2-5*p5) `div` 10)],
            p20 <- [0..((200-p1-2*p2-5*p5-10*p10) `div` 20)],
            p50 <- [0..((200-p1-2*p2-5*p5-10*p10-20*p20) `div` 50)],
            p100 <- [0..((200-p1-2*p2-5*p5-10*p10-20*p20-50*p50) `div` 100)],
            p200 <- [0..1],
            p1 + 2*p2 + 5*p5 + 10*p10 + 20*p20 + 50*p50 + 100*p100 + 200*p200 == 200]

solve31 = sum helper31

-- 32

digitsToN l = digitsToN' l ((length l) - 1)
  where
    digitsToN' [] _ = 0
    digitsToN' (d:ds) n = (d*10^n) + digitsToN' ds (n-1)

helper32' l1 l2 = [(n1,n2) | ls1 <- permutations l1, ls2 <- permutations l2,
                   let n1 = digitsToN ls1,
                   let n2 = digitsToN ls2,
                   test n1 n2 ([1..9] \\ (ls1++ls2))]
  where
    test n1 n2 diff = sort (getDigits (n1*n2)) == diff

helper32 = [ x*y | n <- [1..2], l1 <- f n, l2 <- g n 4 ([1..9] \\ l1), (x,y) <- helper32' l1 l2]
  where
    digs = subsequences [1..9]
    f n = filter (\x -> (length x) == n) digs
    g l u list = filter (\x -> (length x) >= l &&
                               (length x) <= u) digs'
      where digs' = subsequences list

solve32 = sum . nub $ helper32

-- 33

curiousFrac n d
  | (length common) == 1 &&
    (((fromIntegral n)/(fromIntegral d)) == (fromIntegral n'/fromIntegral d')) &&
    ((n `rem` 10 /= 0) || (d `rem` 10 /= 0)) &&
    (n' <= d')
    = Just (n',d')
  | otherwise = Nothing
  where
    l1 = getDigits n
    l2 = getDigits d
    common = l1 `intersect` l2
    n' = head $ l1 \\ common
    d' = head $ l2 \\ common

commonDenom l = fromJust $ find f [m..]
  where
    m = maximum l
    f x = all (\y -> x `rem` y == 0) l

solve33' = [fromJust r | n <- [11..99], d <- [11..99], let r = curiousFrac n d, isJust r]

solve33 = (product $ map fst solve33', product $ map snd solve33') -- TODO simplyfy

-- 34

curiousNumber n = n == n'
  where digs = getDigits n
        n' = sum $ map (\x -> product [2..x]) digs

solve34 = sum $ [x | x <- [3..99999], curiousNumber x]

-- 35

rotations l = take n $ map (take n) (tails $ cycle l)
  where n = length l

circularPrime p = all isPrime [x | x <- (map digitsToN $ rotations (getDigits p))]

solve35' = length $ filter circularPrime $ takeWhile (< 1000000) primes

-- 36

binaryConv n
  | n <= 0 = []
  | otherwise = (n `rem` 2) : binaryConv (n `div` 2)

isPalindromList l = l == (reverse l)

test36 n = isPalindrom n && (isPalindromList $ binaryConv n)

solve36 = sum $ filter test36 [1..1000000]

-- 37

test37' digs = all isPrime [digitsToN l | l <- tail $ inits digs]
test37'' digs = all isPrime [digitsToN l | l <- init $ tails digs]

test37 p = test37' digs && test37'' digs
  where
    digs = getDigits p

solve37 = sum $ take 11 $ filter test37 $ filter (> 7) primes

-- 38

test38 n i = l == [1,2,3,4,5,6,7,8,9]
  where l = sort $ concatMap (\x-> getDigits (x*n)) [1..i]

solve38 = digitsToN $ (getDigits n') ++ (getDigits (n'*2))
  where n' = maximum [n | n <- [1..10000], any (test38 n) [2..2]] -- bravely guessed ;)

-- 39

-- (bad brute force)

helper39' n = (length [(a,b,c)| c <- [1..n], (a,b) <- pyth_triples' 0 (c^2), a > 0, b > 0, a+b+c == n], n)

solve39 = maximum $ map helper39' [1..1000]

-- 40

decimalstream = concatMap getDigits [1..]

solve40 = d 1 * (d (10^1)) * (d (10^2)) * (d (10^3)) * (d (10^4)) * (d (10^5)) * (d (10^6))
  where d n = decimalstream !! (n-1)

-- 41

pandigital n
  | [1..d] == digs = d
  | otherwise = 0
    where
      digs = sort $ getDigits n
      d = length digs

-- TODO: rectify your search space!!!
solve41 = [(n,p) | p <- takeWhile (< 9999999) $ filter (> 97) primes,
                   let n = pandigital p, n >= 3]

-- 42

isInt x = x == fromInteger (round x)

isTriangleNum n = isInt (sqrt (1 + (8 * (fromInteger n))))

triangle_nums = scanl (+) 1 [2..]

readLine42 :: BL.ByteString -> [String]
readLine42 line = map (init . tail . BL.unpack) (BL.split ',' line)

scoreName42 name = toInteger score
  where score = sum $ map (\x -> 1 + (fromJust $ elemIndex x alphabets)) name
        alphabets = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

solve42 = do
  raw <- BL.readFile "words.txt"
  let inp42 =  concatMap readLine42 $ BL.lines raw
  print $ length $ filter isTriangleNum  $ map scoreName42 inp42


-- 43

helper43 = [[d1,d2,d3,d4,d5,d6,d7,d8,d9,d10] |
            d1 <- [1..9],
            d2 <- [0..9] \\ [d1],
            d3 <- [0..9] \\ [d1,d2],
            d4 <- filter (f d2 d3 2) $ [0..9] \\ [d1,d2,d3],
            d5 <- filter (f d3 d4 3) $ [0..9] \\ [d1,d2,d3,d4],
            d6 <- filter (f d4 d5 5) $ [0..9] \\ [d1,d2,d3,d4,d5],
            d7 <- filter (f d5 d6 7) $ [0..9] \\ [d1,d2,d3,d4,d5,d6],
            d8 <- filter (f d6 d7 11) $ [0..9] \\ [d1,d2,d3,d4,d5,d6,d7],
            d9 <- filter (f d7 d8 13) $ [0..9] \\ [d1,d2,d3,d4,d5,d6,d7,d8],
            d10 <- filter (f d8 d9 17) $ [0..9] \\ [d1,d2,d3,d4,d5,d6,d7,d8,d9]]
  where
    f x y d' z = (x*100 + y*10 + z) `rem` d' == 0

solve43 = sum $ map digitsToN $ helper43

-- 44

isPentagonalNum n = isInt (((sqrt (1 + (24 * (fromInteger n)))) + 1) / 6)

helper44 = [(q-r) | q <- pentagonals, r <- takeWhile (<= q) pentagonals,
                    isPentagonalNum (q-r) && isPentagonalNum (q+r)]

solve44 = head helper44

-- 45

triangles = [n * (n+1) `div` 2 | n <- [1..]]

pentagonals = [n * (3*n-1) `div` 2 | n <- [1..]]

hexagonals = [n * (2*n-1) | n <- [1..]]

-- brute force forever ;)
find45 m n p
  | tr == pe && pe == he = (m,n,p)
  | tr > pe = find45 m (n+1) p
  | pe > he = find45 m n (p+1)
  | otherwise = find45 (m+1) n p
  where
    tr = triangles !! m
    pe = pentagonals !! n
    he = hexagonals !! p

solve45 = find45 285 164 142

-- 46

isSquared n = isInt $ sqrt (fromInteger n)

oddComposites = filter (not . isPrime) [3,5..]

helper46 = [x | x <- oddComposites, and [not $ isSquared ((x-y) `div` 2) | y <- takeWhile (< x) primes]]

solve46 = head helper46

-- 47

distinctPrimes n = length $ nub $ primeFactors n

helper47 = [(x,x+1,x+2,x+3) | x <- [1..], distinctPrimes x == 4,
                             distinctPrimes (x+1) == 4,
                             distinctPrimes (x+2) == 4,
                             distinctPrimes (x+3) == 4]
-- 48

solve48 = sum [i^i `rem` 10^10 | i <- [1..1000]]


-- 49

primes49 = takeWhile (< 10000) $ dropWhile (< 1000) primes

groupedPrimes49 = map (\x -> filter (test49 x) primes49) primes49

test49 x y = xdigs == ydigs
  where xdigs = sort $ getDigits x
        ydigs = sort $ getDigits y

combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

solve49 = filter prop2 $ concatMap (combinationsOf 3) $
          filter (\x -> length x >= 3) groupedPrimes49
  where
    prop2 [a,b,c] = (b-a) == (c-b)

-- 50

primes50 = takeWhile (< 10000) primes

gen [] = []
gen ls = ls : (gen (tail ls) ++ gen (init ls))

slice i j ls = ls''
  where ls' = drop i ls
        ls'' = reverse $ drop j $ reverse ls'

gen' ls = [slice x y ls | s <- [0..n-1], x <- [0..s], let y = s - x]
  where n = length ls

solve50 = length $ fromJust $ find f $ gen' primes50
  where f x = s < 1000000 && isPrime s
          where s = sum x
-- 67

readLine :: BL.ByteString -> [Int]
readLine line = map (fromInteger . fst . fromJust . BL.readInteger) (BL.words line)

solve67 = do
  raw <- BL.readFile "triangle.txt"
  let inp67 =  map readLine $ BL.lines raw
  return $ solve18'' inp67


-- 360 (TODO: Optimize everything... Currently everything is fu*** up)

factors n = map g $ sequence pfs'
  where pfs = primeFactors n
        pfs' = map f $ group pfs
        f l = zip (x:l) [0..len]
          where x = head l
                len = length l
        g [] = 1
        g ((x,p):rest) = x^p * g rest

is4kplus3 n
  | n == 3 = True
  | otherwise = (n - 3) `rem` 4 == 0

isEvenPower n
  | n > 1 && n `rem`2 == 0 = isEvenPower (n `div`2)
  | n <= 1 = True
  | otherwise = False

sqrtAndTruncate = (truncate . sqrt . fromIntegral)

pyth_triples bound r = [(x,y) | x <- [bound..f],
                        let y = sqrtAndTruncate (r - x^2),
                        x^2 + y^2 == r]
  where
    f = sqrtAndTruncate (r `div` 2)

coordinates' :: Integer -> [[Integer]]
coordinates' r = [[x,y,z] | x <- [0..(e r)], (y,z) <- pyth_triples'' x r x]
  where
    rp2 = r^2
    e r = sqrtAndTruncate (rp2 `div` 3)

gaussian_mult (a1,b1) (a2,b2) = (abs((a1*a2)-(b1*b2)),(a1*b2 + b1*a2))

gaussian_mults [] = (1,0)
gaussian_mults (g:gs) = gaussian_mult g (gaussian_mults gs)

-- fast version to find pyth_triples with bound r
pyth_triples' b r = filter (\(x,_) -> x >= b) $ foldl helper4 [(1,0)] pfs'
  where pfs = concatMap helper3 $ group $ primeFactors r
        helper (x,y) = [(x,y),(y,x)]
        helper2 (x,y)
          | x <= y = (x,y)
          |otherwise = (y,x)
        helper3 x
          | is4kplus3 (head x) = [product x]
          | otherwise = x
        pfs' = map ((concatMap helper) . (pyth_triples 0)) pfs
        helper4 l1 l2 = sort . nub . (map helper2) $ map gaussian_mults $
                sequence [l1,l2]

pyth_triples'' b d a
  | isSumOfSquares' = filter (\(x,_) -> x >= b) $ foldl helper4 [(1,0)] pfs'
  | otherwise = []
  where ps = (group . sort) $ primeFactors (d - a) ++ primeFactors (d + a)
        pfs = concatMap helper3 ps
        helper (x,y) = [(x,y),(y,x)]
        helper2 (x,y)
          | x <= y = (x,y)
          |otherwise = (y,x)
        helper3 x
          | is4kplus3 (head x) = [product x]
          | otherwise = x
        pfs' = map ((concatMap helper) . (pyth_triples 0)) pfs
        helper4 l1 l2 = sort . nub . (map helper2) $ map gaussian_mults $
                sequence [l1,l2]
        isSumOfSquares' = all even $ map length facs
          where
            facs = filter (\x -> is4kplus3 (head x)) ps

combination_helper1 l =
  case diff of
    0 -> 6
    1 -> 3
    2 -> 1
  where nubbed = nub l
        diff = (length l) - (length nubbed)

combination_helper2 l = 2^n
  where filtered = filter (/= 0) l
        n = length filtered

solve360' coords = sum $ map (\x -> (combination_helper1 x) * (combination_helper2 x) * (sum x))  coords

solve360 = solve360' $ map (map (* 2^10)) $ coordinates' (10^10 `div` 2^10)

-- 361

binaryString n = map intToDigit $ reverse $ binaryConv n

morse_code n = morse' [1] n
  where morse' ls n
          | n > 0 = morse' (ls ++ (map (1 -) ls)) (n-1)
          | otherwise = ls

morse_code' n = morse'' [0] n
  where morse'' ls n
          | n > 0 = morse'' (ls ++ (map (1 -) ls)) (n-1)
          | otherwise = ls

test361 y morse =  map (\x -> (+) 1 $ fromJust $ (findIndex (== x) (map magic361 [1..1000]))) $ map (\x -> binaryToDecimal $ (take x (drop y (morse 20)))) [1..35]

morse_number b = morse_number' 1 1
  where
    morse_number' i acc
      | i >= b = acc `div` 2^(i-b)
      | otherwise = morse_number' (i*2)  acc'
        where
          tmp = (2^(i)-1) - acc
          acc' = (acc*2^(i)) + tmp

morse_number'' b digs = morse_number''' 1 1 digs
  where
    morse_number''' i acc digs
      | i >= b = (acc,i)
      | otherwise = morse_number''' (i*2)  acc' digs
        where
          tmp = (2^(i)-1) - acc
          acc' = ((acc*2^(i)) `rem` (10^digs)) + (tmp `rem` 10^digs)


binaryToDecimal ls = binaryToDecimal' 0 (reverse ls)
  where
    binaryToDecimal' _ [] = 0
    binaryToDecimal' n (l:ls) = l*(2^n) + binaryToDecimal' (n+1) ls


helper361 n = sort . nub $ map (binaryToDecimal . dropWhile (== 0)) $ f l
  where l = morse_code n
        f l = concatMap tails $ inits l

prepareList l = f l1 ++ g l2
  where i = findPair l 0
        (l1,l2) = splitAt i l
        f l'
          | even n' = l'
          | otherwise =  (1 - (head l')) : l'
          where n' = length l'
        g l'
          | even n' = l'
          | otherwise =  l' ++ [(1 - (last l')) ]
            where n' = length l'

reduceSequence l
  | isNothing ml = Nothing
  | length l'' > 3 = reduceSequence l''
  | otherwise = ml
  where ml = reduceSequence' (prepareList l) []
        l'' = fromJust ml
        reduceSequence' [] acc = Just (reverse acc)
        reduceSequence' (0:1:xs) acc = reduceSequence' xs (0:acc)
        reduceSequence' (1:0:xs) acc = reduceSequence' xs (1:acc)
        reduceSequence' _ _ = Nothing

findPair [] _ = -1
findPair [_] _ = -1
findPair (x:y:xs) n
  | x == y = (n+1)
  | otherwise = findPair (y:xs) (n+1)

isMorseSequence [] = True
isMorseSequence [_] = True
isMorseSequence [_,_] = True
isMorseSequence [a,b,c]
  | a == b && b == c = False
  | otherwise = True
isMorseSequence l =
  case (reduceSequence l) of
    Just l' -> isMorseSequence l'
    _       -> False

magic361' ls xs n
  | n < 0 = l''
  | otherwise = magic361' l' (l'') (n-1)
    where
      l' = filter isMorseSequence $ (map (0 :) ls) ++ (map (1 :) ls)
      l'' = filter (\x -> head x == 1) l'

magicStream = 1 : (concat $ zipWith (++) twos ones)
  where ones = [replicate (2^i) 1 | i <- [0..]]
        twos = [replicate (2^i) 2 | i <- [0..]]

calcMagic b = (calcMagic' magicStream 2 0 2 b)
  where
    calcMagic' (o:os) p q n b
      | n >= b = (p,2+(q+p))
      | otherwise = calcMagic' os (p+o) (q+p) (n+1) b

calcMagic'' n = calcMagic''' 7 3 1 3
  where
    calcMagic''' acc bits i p
      | acc >= n = (acc,bits)
      | otherwise = calcMagic''' (acc+t1+t2) (bits + (i*2)) (i*2) p''
        where
          t1 = p*i + i*(i+1)
          p' = p + (2*i)
          t2 = p'*i + (i*(i+1) `div` 2)
          p'' = p' + i

magic361 b = binaryToDecimal $ magic361'' [[]] 1 b

magic361'' ls n b
  | n + n' > b = l'' !! (b - n)
  | otherwise = magic361'' l' (n+n') b
    where
      l' = filter isMorseSequence $ (map (0 :) ls) ++ (map (1 :) ls)
      l'' = filter (\x -> head x == 1) l'
      n' = length l''

expand361 ls = concatMap f ls
  where
    f 0 = [0,1]
    f 1 = [1,0]


parity n = parity' n 0
  where
    parity' n acc
      | n <= 0 = acc
      | (n `rem` 2) == 1 =  parity' (n `div` 2) (acc+1)
      | otherwise = parity' (n `div` 2) acc


{--
morse_number'' i acc
      | i >= b = acc
      | even p = morse_number'' (i+1) (acc*2+1)
      | otherwise = morse_number'' (i+1)  (acc*2)
        where
          p = parity i
--}
-- map (\x -> zipWith (-) (tail x) x) $ map (map binaryToDecimal) (map (magic361' [[]] []) [1..20])

--  map (\x -> (+) 1 $ fromJust $ (findIndex (== x) (map magic361 [1..200]))) $ (\y -> map (\x -> binaryToDecimal $ (take x (drop y (morse_code 10)))) [1..14]) $ (findIndices (== 1) (morse_code 5)) !! 4


-- yo mama

fac n = product [2..n]

binomial n k = (fac n) `div` ((fac k) * fac (n-k))

foo362 n = 1 + (sum $ map (\k -> (binomial n k) * foo362 (n-k)) [2..n])

bar362 n = length . nub $ primeFactors n

