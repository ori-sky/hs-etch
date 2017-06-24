module Etch.Parser where

import Control.Applicative (many)
import Data.ByteString.Char8
import Data.Attoparsec.ByteString.Char8 hiding (parse)
import Etch.AST
import qualified Etch.Lexer as Lexer

integerLiteralParser :: Parser AST
integerLiteralParser = IntegerLiteral <$ skipSpace <*> Lexer.integerParser <* skipSpace

identifierParser :: Parser AST
identifierParser = integerLiteralParser

definitionParser :: Parser AST
definitionParser = Definition <$> identifierParser <* char '=' <*> identifierParser

astParser :: Parser AST
astParser = definitionParser

parse :: ByteString -> Either String [AST]
parse = parseOnly (many astParser)
