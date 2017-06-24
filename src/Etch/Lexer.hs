module Etch.Lexer where

import Data.Attoparsec.ByteString.Char8

integerParser :: Parser Integer
integerParser = signed decimal
