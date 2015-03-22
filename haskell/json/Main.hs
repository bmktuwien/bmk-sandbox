module Main where

import Json

main = do
  print (JObject [("foo", JNumber 1), ("bar", JBool False)])

-- EOF --
