{- Question:
   Write a pattern checker. The pattern checker takes a pattern and an arbitrary
   string as input and checks if the string matches the given pattern.

   Example:

   "abba" "RedBlueBlueRed"  --> True
   "aaaa" "RedRedRedBlue"   --> False
-}

import           Data.List
import qualified Data.HashMap.Strict as HM

patternCheck :: String -> String -> Bool
patternCheck = patternCheck' HM.empty

patternCheck' :: HM.HashMap Char String -> String -> String -> Bool
patternCheck' _ [] [] = True
patternCheck' _ _ []  = False
patternCheck' _ [] _  = False
patternCheck' bindings (p:ps) s
  | Just b <- p `HM.lookup` bindings =
    isPrefixOf b s && patternCheck' bindings ps (s' b)
  | otherwise =
    or [patternCheck' (HM.insert p b bindings) ps (s' b) | b <- tail $ inits s ]
  where
    s' b = drop (length b) s
