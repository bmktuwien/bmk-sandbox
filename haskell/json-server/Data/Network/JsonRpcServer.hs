{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Network.JsonRpcServer where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text        as T

data JsonRpcRequest = JsonRpcRequest
  { jrReqId     :: !Value
  , jrReqVer    :: !Value
  , jrReqMethod :: !T.Text
  , jrReqParams :: !Value
  } deriving (Show)

data JsonRpcResponse r =
    JsonRpcResult { jrResId     :: !Value
                  , jrResVer    :: !Value
                  , jrResResult :: r
                  }
  | JsonRpcError  { jrErrId      :: !Value
                  , jrErrVer     :: !Value
                  , jrErrCode    :: !Int
                  , jrErrMessage :: !T.Text
                  , jrErrData    :: !Value
                  }
  deriving (Show)

isValidJsonRpcVer :: Value -> Bool
isValidJsonRpcVer (String "2.0") = True
isValidJsonRpcVer (Null)         = True
isValidJsonRpcVer _              = False

instance FromJSON JsonRpcRequest where
  parseJSON (Object v) = do
    jver    <- v .:? "jsonrpc" .!= Null -- if null it's jsonrpc 1.0
    jid     <- v .:? "id" .!= Null      -- if null it's a notification
    method  <- v .:  "method"
    params  <- v .:? "params" .!= emptyObject

    unless (isValidJsonRpcVer jver) mzero

    case params of
      (Array _)  -> return ()
      (Object _) -> return ()
      _          -> mzero

    return $ JsonRpcRequest jid jver method params

  parseJSON _ = mzero

instance (FromJSON r) => FromJSON (JsonRpcResponse r) where
  parseJSON (Object v) = parseResult `mplus` parseError
    where
      parseResult = do
        jver   <- v .:? "jsonrpc" .!= Null
        jid    <- v .: "id"
        result <- v .: "result"

        unless (isValidJsonRpcVer jver) mzero

        return $ JsonRpcResult jid jver result

      parseError = do
        jver         <- v .:? "jsonrpc" .!= Null
        jid          <- v .: "id"
        errorObj     <- v .: "error"
        errorCode    <- errorObj .: "code"
        errorMessage <- errorObj .: "message"
        errorData    <- errorObj .:? "data" .!= Null

        unless (isValidJsonRpcVer jver) mzero

        return $ JsonRpcError jid jver errorCode errorMessage errorData

  parseJSON _ = mzero

instance ToJSON JsonRpcRequest where
  toJSON JsonRpcRequest{..} = object [ "jsonrpc" .= jrReqVer
                                     , "id"      .= jrReqId
                                     , "method"  .= jrReqMethod
                                     , "params"  .= jrReqParams
                                     ]

instance ToJSON r => ToJSON (JsonRpcResponse r) where
  toJSON (JsonRpcResult jid jver result) =
    object [ "jsonrpc" .= jver
           , "id"      .= jid
           , "result"  .= result
           ]

  toJSON (JsonRpcError jid jver errCode errMessage errData) =
    object [ "jsonrpc" .= jver
           , "id"      .= jid
           , "error"   .= errObj
           ]
    where
      errObj = object [ "code"    .= errCode
                      , "message" .= errMessage
                      , "data"    .= errData
                      ]
