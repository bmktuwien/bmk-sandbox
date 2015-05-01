{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Blaze.ByteString.Builder (copyByteString)
import           Control.Concurrent       (runInUnboundThread)
import           Data.Aeson               (encode, object, (.=))
import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Lazy     as L
import           Data.Text                (Text)
import           Network.HTTP.Types       (hContentLength, status200, status404)
import           Network.Wai              (rawPathInfo, responseBuilder,
                                           responseLBS)
import qualified Network.Wai.Handler.Warp as W

main :: IO ()
main =
    runInUnboundThread $ W.runSettings settings app
  where
    settings = W.setPort 8888
             $ W.setOnException (\_ _ -> return ()) W.defaultSettings

    app request respond = case rawPathInfo request of
        "/json" -> respond responseJson
        "/plaintext" -> respond responsePlaintext
        _ -> respond $ responseBuilder status404 [] ""
    !responseJson = responseBuilder status200 ctJson json

    ctJson = [("Content-Type", "application/json")]

    !json = copyByteString
          $ L.toStrict
          $ encode
          $ object ["message" .= ("Hello, World!" :: Text)]

    !responsePlaintext = responseLBS status200 ctPlaintext plaintext

    ctPlaintext = [("Content-type", "text/plain"), ("Connection", "keep-alive"),
                   (hContentLength,  B8.pack . show $ L.length plaintext)]

    plaintext = "Hello, World!" :: L.ByteString
