{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Network.JsonRpcServer where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text        as T

data JsonRpcRequest = JsonRpcRequest
  { jrReqId     :: !Value
  , jrReqMethod :: !T.Text
  , jrReqParams :: !Value
  } deriving (Show)

data JsonRpcResponse r =
    JsonRpcResult { jrResId     :: !Value
                  , jrResResult :: r
                  }
  | JsonRpcError  { jrErrId      :: !Value
                  , jrErrCode    :: !Int
                  , jrErrMessage :: !T.Text
                  , jrErrData    :: !Value
                  }
  deriving (Show)

instance FromJSON JsonRpcRequest where
  parseJSON (Object v) = do
    String "2.0" <- v .:  "jsonrpc"
    jid          <- v .:? "id" .!= Null -- if null it's a notification
    method       <- v .:  "method"
    params       <- v .:? "params" .!= emptyObject

    case params of
      (Array _)  -> return ()
      (Object _) -> return ()
      _          -> mzero

    return $ JsonRpcRequest jid method params

  parseJSON _ = mzero

instance (FromJSON r) => FromJSON (JsonRpcResponse r) where
  parseJSON (Object v) = parseResult `mplus` parseError
    where
      parseResult = do
        String "2.0"  <- v .:  "jsonrpc"
        jid           <- v .: "id"
        result        <- v .: "result"
        return $ JsonRpcResult jid result

      parseError = do
        String "2.0" <- v .:  "jsonrpc"
        jid          <- v .: "id"
        errorObj     <- v .: "error"
        errorCode    <- errorObj .: "code"
        errorMessage <- errorObj .: "message"
        errorData    <- errorObj .:? "data" .!= Null
        return $ JsonRpcError jid errorCode errorMessage errorData

instance ToJSON JsonRpcRequest where
  toJSON JsonRpcRequest{..} = object [ "jsonrpc" .= String "2.0"
                                     , "id"      .= jrReqId
                                     , "method"  .= jrReqMethod
                                     , "params"  .= jrReqParams
                                     ]

instance ToJSON r => ToJSON (JsonRpcResponse r) where
  toJSON (JsonRpcResult jid result) =
    object [ "jsonrpc" .= String "2.0"
           , "id"      .= jid
           , "result"  .= result
           ]

  toJSON (JsonRpcError jid errCode errMessage errData) =
    object [ "jsonrpc" .= String "2.0"
           , "id"      .= jid
           , "error"   .= errObj
           ]
    where
      errObj = object [ "code"    .= errCode
                      , "message" .= errMessage
                      , "data"    .= errData
                      ]
