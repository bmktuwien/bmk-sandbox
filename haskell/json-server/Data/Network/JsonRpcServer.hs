{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Network.JsonRpcServer where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Network.HTTP.Types
import           Network.Wai

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

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

type JsonRPCFunc = Value -> JsonRpcResponse Value

data JsonRPCRoute = JsonRPCRoute
  { jrRouteDomain :: !T.Text
  , jrRouteMethod :: !T.Text
  , jrRouteFunc   :: JsonRPCFunc
  }

type JsonRPCRouteMap = HM.HashMap (T.Text, T.Text) JsonRPCFunc

mkRouteMap :: [JsonRPCRoute] -> JsonRPCRouteMap
mkRouteMap = HM.fromList . map f
  where
    f JsonRPCRoute{..} = ((jrRouteDomain, jrRouteMethod), jrRouteFunc)

jsonRPCServer :: JsonRPCRouteMap -> Application
jsonRPCServer routeMap request respond
  | requestMethod request == methodPost = handle errorHandler $ do
      eRequest <- eitherDecode' . BL.fromStrict <$> requestBody request
      case eRequest of
        Left _ -> respond $ mkHTTPErrorResp status400
        Right JsonRpcRequest{..} -> do
          let domain = T.decodeUtf8 $ rawPathInfo request
              response = case HM.lookup (domain,jrReqMethod) routeMap of
                Just rpcFunc -> rpcFunc jrReqParams
                Nothing      -> JsonRpcError jrReqId jrReqVer (-32601)
                                "Method not found" Null

          respond . responseLBS status200 [] $ encode response
  | otherwise = respond $ mkHTTPErrorResp status405 -- method not allowed

  where
    mkHTTPErrorResp status = responseLBS status [] BL.empty

    errorHandler :: SomeException -> IO ResponseReceived
    errorHandler _ =
      respond $ mkHTTPErrorResp status500 -- internal server error
