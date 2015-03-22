module PutJSON where

import Data.List (intercalate)
import Json

renderJValue :: JValue -> String
renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue (JNull)       = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values ++ "]"
  where values = intercalate ", " (map renderJValue a)


putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

-- EOF --
