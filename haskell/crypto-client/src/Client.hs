{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import qualified Data.ByteString.Char8  as BS
import           Network.Http.Client
import           System.Console.CmdArgs
import           GKSL

data ClientConfig = ClientConfig
    { cfgHost  :: String
    , cfgPort  :: Int
    } deriving (Data, Typeable, Show)

cmdLineConfig :: IO ClientConfig
cmdLineConfig  = cmdArgs $ ClientConfig
  { cfgHost = "0.0.0.0"   &= argPos 0 &= typ "HOST"
  , cfgPort    = 8000     &= argPos 1 &= typ "PORT"
  } &= program "crypto-client"


openConnection' :: ClientConfig -> IO Connection
openConnection' cfg = do
  let h = BS.pack $ cfgHost cfg
      p = fromIntegral $ cfgPort cfg

  openConnection h p

------------------------------------------------------------------

main :: IO ()
main = do
  -- read cmdline options
  config <- cmdLineConfig

  conn <- openConnection' config

  fhmqvKeyExchange conn
