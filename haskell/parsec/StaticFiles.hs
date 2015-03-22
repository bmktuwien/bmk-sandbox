{-# LANGUAGE OverloadedStrings #-}

import           Data.Char
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.Combinator       as P
import qualified Data.Map                         as Map
import qualified Data.ByteString.Char8            as B
import qualified Data.Aeson                       as J
import qualified Data.HashMap.Strict              as HMap
import qualified Data.Text                        as T

import qualified Text.Parsec as TP
import qualified Text.Parsec.ByteString as TP
import Control.Applicative
import Data.Maybe
import Control.Monad


type StaticFileEntry = (B.ByteString,B.ByteString,B.ByteString,[(B.ByteString,B.ByteString)])

parsecStaticFileMap :: TP.Parser [StaticFileEntry]
parsecStaticFileMap = catMaybes <$> TP.endBy line (TP.many1 TP.newline)
  where
    line = spaces *> (entry <|> emptyLine)

    emptyLine = TP.optional comment *> return Nothing

    entry = liftM Just $ (,,,) <$>
            (token TP.<?> "url-path")     <*>
            (token TP.<?> "content-type") <*>
            (token TP.<?> "file-path")    <*>
            TP.option [] headers <*
            TP.optional comment

    token  = B.pack <$> TP.many1 (TP.noneOf "# \t\n") <* spaces

    comment = TP.char '#' >> TP.manyTill TP.anyChar (TP.lookAhead TP.newline)
              TP.<?> "comment"

    spaces = TP.many $ TP.oneOf " \t"

    headers :: TP.Parser [(B.ByteString, B.ByteString)]
    headers = series '{' headers' '}' <* spaces TP.<?> "custom-headers"
      where
        headers' = (,) <$> string <* spaces <* TP.char ':' <* spaces <*> string

        series left parser right = TP.between (TP.char left <* spaces) (TP.char right) $
                                   (parser <* spaces) `TP.sepBy` (TP.char ',' <* spaces)

        string = B.pack <$> TP.between (TP.char '\"') (TP.char '\"')
                 (TP.many $ TP.noneOf "\"\\")

parseFoo :: B.ByteString -> Either TP.ParseError [StaticFileEntry]
parseFoo input = TP.parse parsecStaticFileMap "(unknown)" input
