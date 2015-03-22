{-# LANGUAGE CPP, BangPatterns, OverloadedStrings, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import           Data.Int
import           Data.Bits
import           Data.Word
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Text.Encoding (encodeUtf8)

import           Test.Framework (Test, defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck (Arbitrary(..), Gen, Property, elements, frequency, vectorOf)
import           Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

import           Data.BJSOS

import qualified Data.Json.Builder as JB
import qualified Data.Aeson as J

import Control.Concurrent (newEmptyMVar, forkIO, putMVar, takeMVar)
import qualified Control.Exception as C
import Control.Monad (when)
import           System.IO
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.IO.Exception (IOErrorType(OtherError))
import System.Exit (ExitCode(..))
import System.IO (hFlush, hClose)
import System.IO.Error (mkIOError)
import System.Process (CreateProcess(std_in, std_out, std_err, cwd), createProcess, waitForProcess, proc, StdStream(CreatePipe, Inherit), showCommandForUser)

main :: IO ()
main = do
  defaultMain tests

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary

-- the following instances are orphans here because they are just a hack for testing
deriving instance Eq JValue
instance Eq JRaw where _ == _  = False

-- | 'Integer' type with specific 'Arbitrary' instance generating values
-- exceeding the 'Int64' domain
newtype BigInt = BigInt Integer
               deriving (Show,Eq,Ord,Num,ToBJSOS,FromBJSOS)

instance Arbitrary BigInt where
    arbitrary = BigInt <$> bigIntGen

bigIntGen :: Gen Integer
bigIntGen = elements $ filter isBigInt [ s*(bit i + j) | j <- [-2..2], s <- [-1,1], i <- [63..200] ]

isBigInt :: Integer -> Bool
isBigInt = (== Nothing) . integer2Int64

tests :: [Test]
tests = [ testGroup "{from/to}BJSOS" [ -- testProperty "Double"  (testRoundtrip :: Double -> Bool)
                                     -- , testProperty "Int"     (testRoundtrip :: Int    -> Bool)
                                     -- , testProperty "Int8"    (testRoundtrip :: Int8   -> Bool)
                                     -- , testProperty "Int16"   (testRoundtrip :: Int16  -> Bool)
                                     -- , testProperty "Int32"   (testRoundtrip :: Int32  -> Bool)
                                     -- , testProperty "Int64"   (testRoundtrip :: Int64  -> Bool)
                                     -- , testProperty "Word"    (testRoundtrip :: Word   -> Bool)
                                     -- , testProperty "Word8"   (testRoundtrip :: Word8  -> Bool)
                                     -- , testProperty "Word16"  (testRoundtrip :: Word16 -> Bool)
                                     -- , testProperty "Word32"  (testRoundtrip :: Word32 -> Bool)
                                     -- , testProperty "Word64"  (testRoundtrip :: Word64 -> Bool)
                                     -- , testProperty "Integer" (testRoundtrip :: Integer -> Bool)
                                     -- , testProperty "BigInt"  (testRoundtrip :: BigInt -> Bool)
                                     -- , testProperty "Bool"    (testRoundtrip :: Bool   -> Bool)
                                     -- , testProperty "[[Maybe Bool]]" (testRoundtrip :: [[Maybe Bool]] -> Bool)
                                     -- , testProperty "Text"    (testRoundtrip :: T.Text -> Bool)
                                     -- , testProperty "(Bool,Int)" (testRoundtrip :: (Bool,Int) -> Bool)
                                     -- , testProperty "(Text,Bool,Int)" (testRoundtrip :: (T.Text,Bool,Int) -> Bool)
                                     -- , testProperty "(Int,Int,Int,Int)" (testRoundtrip :: (Int,Int,Int,Int) -> Bool)
                                     -- , testProperty "(Int,Int,Int,Int,Int)" (testRoundtrip :: (Int,Int,Int,Int,Int) -> Bool)
                                     -- -- missing arbitrary instances
                                     -- -- , testProperty "(Int,Int,Int,Int,Int,Int)" (testRoundtrip :: (Int,Int,Int,Int,Int,Int) -> Bool)
                                     -- -- , testProperty "(Int,Int,Int,Int,Int,Int,Int)" (testRoundtrip :: (Int,Int,Int,Int,Int,Int,Int) -> Bool)
                                     -- , testProperty "JValue"  (testRoundtrip :: JValue -> Bool)
                                     testProperty "Foo" testFoo
                                     ]
        , testProperty "encode'/decode'" testEncDec
        , testProperty "decodeCheckLength" testDecodeCheckLength

        , testProperty "JSON" testJson
        ]

testRoundtrip :: (Eq a, ToBJSOS a, FromBJSOS a) => a -> Bool
testRoundtrip v = Just v == (fromBJSOS . toBJSOS) v

instance Arbitrary JValue where
    arbitrary = frequency [ (100, pure JNull)
                          , (100, pure JTrue)
                          , (100, pure JFalse)
                          , (200, JInt <$> arbitrary)
                          , (200, smartJDecimal <$> bigIntGen <*> pure 0)
                          , (200, smartJDecimal <$> arbitrary <*> arbitrary)
                          , (200, JDouble <$> arbitrary)
                          , (200, JString . encodeUtf8 . T.pack <$> arbitrary)
                          , (200, JBlob   <$> arbitrary)

                          , ( 50, JArray <$> vectorOf 1 arbitrary)
                          , ( 50, JArray <$> vectorOf 2 arbitrary)
                          , ( 25, JArray <$> vectorOf 3 arbitrary)
                          , ( 25, JArray <$> vectorOf 4 arbitrary)
                          , ( 25, JArray <$> vectorOf 5 arbitrary)

                          , ( 50, JObject <$> vectorOf 1 kvArb)
                          , ( 50, JObject <$> vectorOf 2 kvArb)
                          , ( 25, JObject <$> vectorOf 3 kvArb)
                          , ( 25, JObject <$> vectorOf 4 kvArb)
                          , ( 25, JObject <$> vectorOf 5 kvArb)

                            -- no JRaw
                          ]
      where
        kvArb = (\k v -> (encodeUtf8 (T.pack k), v)) <$> arbitrary <*> arbitrary

smartJDecimal :: Integer -> Word8 -> JValue
smartJDecimal n e
  | e == 0, Just i <- integer2Int64 n = JInt i
  | otherwise               = JDecimal n e

integer2Int64 :: Integer -> Maybe Int64
integer2Int64 i
  | minInt64 <= i && i <= maxInt64  = Just (fromIntegral i)
  | otherwise                       = Nothing
  where
    minInt64 = toInteger (minBound :: Int64)
    maxInt64 = toInteger (maxBound :: Int64)

testEncDec :: JValue -> Bool
testEncDec v = Right v == (decode' . encode') v

testDecodeCheckLength :: JValue -> Bool
testDecodeCheckLength v = 0 == (decodeCheckLength . encode') v

-- count nodes
jvalueSize :: Num a => JValue -> a
jvalueSize (JArray vs) = sum (map jvalueSize vs)
jvalueSize (JObject kvs) = sum (map (jvalueSize . snd) kvs)
jvalueSize _ = 1

-- test that "toJsonBlob == eitherDecode . toJsonLBS"
testJson :: JValue -> Bool
testJson v0 = j1 == j2
  where
    j1 = toJsonBlob defaultJsonBlobHandler v
    j2 = either error id $ J.eitherDecode' $ JB.toJsonLBS v

    -- eitherDecode needs [] or {} at the top-level
    v = case v0 of
        JArray _  -> v0
        JObject _ -> v0
        _         -> JArray [v0]

toJArrayOrJObject v = case v of
  JArray  _ -> v
  JObject _ -> v
  _         -> JArray [v]

testFoo :: JValue -> Property
testFoo v = monadicIO $ do
  let j = JB.toJsonLBS $ toJArrayOrJObject v
  readData <- run $ readProcess "json2bjsos" [] id j
  run $ print readData
  assert $ True


readProcess
    :: FilePath		-- ^ command to run
    -> [String]		-- ^ any arguments
    -> (CreateProcess -> CreateProcess) -- ^ modifies the CreateProcess record before passing it to createProcess.  Use 'id' for the normal 'System.Process.readProcess' behavior
    -> BL.ByteString	-- ^ standard input
    -> IO BL.ByteString	-- ^ standard output
readProcess cmd args modify input = do
    let modify' p = modify (p {std_in  = CreatePipe, std_out = CreatePipe, std_err = Inherit })
    (Just inh, Just outh, _, pid) <-
        createProcess (modify' (proc cmd args))

    -- fork off a thread to start consuming the output
    output  <- BL.hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ C.evaluate (BL.length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (BL.null input)) $ do BL.hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
      ExitSuccess   -> return output
      ExitFailure r -> fail $ show r
