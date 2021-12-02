{-# LANGUAGE RecordWildCards #-}

module Parser (
  Parser,
  module Text.Megaparsec,
  module Text.Megaparsec.Char,
  runMyParser,
  lexeme,
  symbol,
  sc,
) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Data.Text (Text)
import Data.Void (Void)

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--                   ^    ^
--                   |    \ Token stream
--                   |
--                   \ Custom error type

runMyParser :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runMyParser parser =
  parse parser "<stdio>"

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
