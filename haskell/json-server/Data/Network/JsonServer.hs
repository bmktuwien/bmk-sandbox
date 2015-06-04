{-# LANGUAGE OverloadedStrings #-}

module Data.Network.JsonServer where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text        as T

data JsonRpcRequest a = JsonRpcRequest
  { jrrMethod :: T.Text
  , jrrParams :: Maybe a
  , jrrId     :: Maybe Int
  } deriving (Show)


instance FromJSON a => FromJSON (JsonRpcRequest a) where
  parseJSON (Object v) = do
    "2.0"  <- (v .:  "jsonrpc") :: Parser T.Text
    jid   <- v .:? "id"
    method <- v .:  "method"
    params <- v .:? "params"
    return $ JsonRpcRequest method params jid

  parseJSON _ = mzero
