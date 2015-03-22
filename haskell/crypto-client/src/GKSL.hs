{-# LANGUAGE OverloadedStrings #-}

module GKSL
       ( rsaKeyExchange
       , fhmqvKeyExchange
       ) where

import qualified Blaze.ByteString.Builder     as BB
import           Control.Monad
import qualified Crypto.PubKey.RSA            as RSA
import qualified Crypto.PubKey.RSA.PKCS15     as PKCS15
import           Crypto.Random.API
import qualified CryptoPP.AES                 as AES
import qualified CryptoPP.FHMQV               as FHMQV
import qualified CryptoPP.RNG                 as RNG
import qualified CryptoPP.SHA                 as SHAPP
import qualified Data.Binary.Get              as BG
import           Data.Bits
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B8
import qualified Data.ByteString.Base16       as Base16
import qualified Data.ByteString.Char8        as BC
import qualified Data.ByteString.Lazy         as BL
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Data.Tuple
import           Data.Word
import           Network.Http.Client
import qualified System.IO.Streams            as Streams
import           System.IO.Streams.ByteString (readExactly)
import           System.IO.Unsafe
import           Text.Printf


type SessionId = Word64

newtype MsgTag = MsgTag Word32
               deriving Show

data Msg = Msg !MsgTag !SessionId ByteString
         deriving Show


-- static public key of gatekeeper 3
gksPubKey :: FHMQV.StaticPublicKey
gksPubKey = decodeHex "49a84fba54e85610ca492f824391d05b63e1d8ceb2cb40b9b5e9eb2023e65a6aec73345f42a89a2f55ad153fcc7d9703375b4ba5d3a65f54a1cd517d067cab461h"

--gksPrivKey :: FHMQV.StaticPrivateKey
--gksPrivKey = decodeHex "c7b78e17fc2a640cd8236c1bbac1715ecaeeb7bdace618af4eb49e3367c261aah"

-- static public key of crypto-client
--sPubKey :: FHMQV.StaticPublicKey
--sPubKey = decodeHex "4c0e45a14cd312f0f145a8ef65bb0d536323189c653921cc7a21830ce50540e538500b66d04997cee9e0642b1f2e7e634ecaa0566d01a4818272a8adbbd20eb73h"

sPrivKey :: FHMQV.StaticPrivateKey
sPrivKey = decodeHex "ca08ce348991830ee3f19a92654ae00a43f5ee65052e98722d7a174fa634162h"


msgLength :: Integral a => Msg -> a
msgLength (Msg _ _ payload) = fromIntegral $ (B8.length payload) + 16

encodeMsg :: Msg -> BB.Builder
encodeMsg (Msg (MsgTag tag) sid payload) =
    mconcat [ BB.fromWord32be tag
            , BB.fromWord32be (fromIntegral $ B8.length payload)
            , BB.fromWord64be sid
            , BB.fromByteString payload
            ]

decodeHex :: ByteString -> ByteString
decodeHex bs
  | B8.null e = d
  | otherwise = error "not a valid hex-encoded string"
  where
    (d,e) = Base16.decode bs

decodeMsg :: BG.Get Msg
decodeMsg = do
    tag <- BG.getWord32be
    l   <- BG.getWord32be
    sid <- BG.getWord64be
    pl  <- BG.getByteString (fromIntegral l)
    return $! Msg (MsgTag tag) sid pl

decodeMsg' :: ByteString -> Either String Msg
decodeMsg' = runGet' decodeMsg
{-# INLINE decodeMsg' #-}

runGet' :: BG.Get b -> ByteString -> Either String b
runGet' d s = case BG.runGetOrFail d (BL.fromStrict s) of
    Right (s',_,v) | BL.null s' -> Right v
                   | otherwise  -> Left "trailing garbage"
    Left (_,_,e) -> Left e
{-# INLINE runGet' #-}


sysCPRG :: IORef SystemRandom
sysCPRG = unsafePerformIO $ newIORef =<< getSystemRandomGen
{-# NOINLINE sysCPRG #-}

encryptRSAPKCS15 :: RSA.PublicKey -> ByteString -> IO ByteString
encryptRSAPKCS15 pk bs = do
  ct <- atomicModifyIORef' sysCPRG f
  case ct of
    Right ct' -> return ct'
    Left _    -> fail "error occured during rsa encryption"
  where
    f g = swap $ PKCS15.encrypt g pk bs


genRandomBS :: Int -> IO ByteString
genRandomBS n = atomicModifyIORef' sysCPRG (swap . genRandomBytes n)


bsToInteger :: ByteString -> Integer
bsToInteger = B8.foldl' f 0
  where
    f a w = (a `shiftL` 8) .|. fromIntegral w

getPTime :: IO Word32
getPTime = fmap floor getPOSIXTime

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

doRequest :: Connection -> Msg -> IO ()
doRequest conn msg = do
  req <- buildRequest $ do
    http POST "/"
    setContentLength $ msgLength msg

  sendRequest conn req (msgWriter msg)

  where
    msgWriter m = Streams.write (Just $ encodeMsg m)

getResponse :: Connection -> IO Msg
getResponse conn = do
  receiveResponse conn $ \p i -> do
    unless (getStatusCode p == 200) $
      fail "unexpected status-code"

    n <- case getHeader p "Content-Length" of
      Just x' -> return (fst . fromJust $ BC.readInt x')
      Nothing -> fail "missing content-length"

    mb <- readExactly n i

    case decodeMsg' mb of
      Right msg' -> return msg'
      Left _     -> fail "decoding server response failed"

{-- method 0x01 (RSA key exchange) -}

-- TODO: not yet fully implemented
rsaKeyExchange :: Connection -> IO ()
rsaKeyExchange conn = do
  -- ***** initiate ClientHello
  -- tag:0x0001 len:0x0001 sid:0x0000 payload:0x01
  printf "sending ClientHello(RSA-KE) ...\n"

  let chMsg = Msg (MsgTag 0x1) 0x0 $ B8.pack [0x1]

  doRequest conn chMsg

  -- ***** server response to client-hello
  (Msg (MsgTag mtag) sid pay) <- getResponse conn
  let rsaPubN = bsToInteger pay

  unless (mtag == 0x4) $
      fail "unexpected server message tag"

  unless (B8.length pay == 256) $
    fail "unexpected rsa pub key length"

  printf "Pre-SID: 0x%x\n" sid
  printf "RSA public n = %d\n" rsaPubN

  -- ***** send session request
  sessionIV0 <- genRandomBS 16
  sessionKey <- genRandomBS 32
  let threadCnt = 4 :: Int
      pubKey = RSA.PublicKey 256 rsaPubN 0x10001

  printf "session-iv   = %x\n" (bsToInteger sessionIV0)
  printf "session-key  = %x\n" (bsToInteger sessionKey)
  printf "thread-count = %d\n" threadCnt


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-- method 0x02 (FHMQV key exchange) -}

calcCHPayload :: Word8 -> Word8 -> Word16 -> Word32 -> Word64 -> ByteString -> ByteString
calcCHPayload method threadCnt paramId time salt ePubKey
  | ok        = input
  | otherwise = calcCHPayload method threadCnt paramId time (salt + 1) ePubKey
  where
    input = (BB.toByteString $ mconcat [ BB.fromWord8 method -- method-id
                                       , BB.fromWord8 threadCnt -- thread-cnt
                                       , BB.fromWord16be paramId -- parameter-id
                                       , BB.fromWord32be time  -- timestamp
                                       , BB.fromWord64be salt  -- pow-salt
                                       ])
            `B8.append` ePubKey

    digest = SHAPP.sha512Digest input

    ok = all (== 0) $ f . B8.unpack $ B8.take 3 digest

    f [w1,w2,w3] = [w1,w2, w3 `shiftR` 7]
    f _          = fail "expected 3 words"


-- TODO: not yet fully implemented
fhmqvKeyExchange :: Connection -> IO ()
fhmqvKeyExchange conn = do
  -- setup
  feh <- FHMQV.makeFhmqvEcp 1 -- client
  rgh <- RNG.makeRandGen --FIXME: instaniate this only once
  (ePrivKey, ePubKey) <- FHMQV.fhmqvEcpGenEphemeralKeyPair feh rgh

  -- ***** initiate ClientHello
  printf "sending ClientHello(FHMQV-KE) ...\n"
  ts <- getPTime

  -- start with salt set to 0
  let threadCnt = 0x4
      paramId   = 0x1
      chPayload = calcCHPayload 0x2 threadCnt paramId ts 0 ePubKey
      chMsg     = Msg (MsgTag 0x1) 0x0 chPayload

  print $ B8.length ePubKey
  print $ Base16.encode chPayload

  doRequest conn chMsg

  printf "thread count        : %x\n" threadCnt
  printf "parameter-id        : %x\n" paramId
  printf "time-stamp          : %x\n" ts
  printf "ephermeal public key: %x\n" (bsToInteger ePubKey)
  printf "-------------------------------------------------------\n"

  -- ***** server response to client-hello
  (Msg (MsgTag mtag) sid pay) <- getResponse conn

  unless (mtag == 0x4) $
      fail "unexpected server message tag"

  let iv0        = B8.take 16 pay
      atag       = B8.take 16 $ B8.drop 16 pay
      encIVs     = B8.take (fromIntegral $ 16 * threadCnt) $
                   B8.drop 32 pay --aes-gcm encrypted
      gkePubKey = B8.drop (fromIntegral $ 16 * threadCnt + 32) pay --server ePubKey

      agreedKey   = FHMQV.fhmqvEcpCalcSharedKey feh sPrivKey ePrivKey gksPubKey gkePubKey
      gkAESGCMKey = B8.take 32 agreedKey
      aesGCMKey   = B8.drop 32 agreedKey


      (ivs,success) = AES.aes256GcmDecrypt gkAESGCMKey iv0
                      (BB.toByteString $ BB.fromWord64be sid) encIVs atag

  -- check some post-conditions
  unless success $
    fail "aes-gcm decrypting of the ivs failed"

  unless (B8.length encIVs == B8.length ivs) $
    fail "decrypted ivs has not the same length as the encrypted ivs"

  printf "SID                              : %x\n" sid
  printf "agreed key                       : %x\n" (bsToInteger agreedKey)
  printf "gatekeeper ephermeal public key  : %x\n" (bsToInteger ePubKey)
  printf "client aes-gcm encryption key    : %x\n" (bsToInteger aesGCMKey)
  printf "gatekeeper aes-gcm encryption key: %x\n" (bsToInteger gkAESGCMKey)

  printf "-------------------------------------------------------\n"
