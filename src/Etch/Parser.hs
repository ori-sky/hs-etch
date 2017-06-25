module Etch.Parser where

import Control.Applicative ((<|>), many)
import Data.ByteString.Char8
import Data.Attoparsec.ByteString.Char8 hiding (parse)
import Etch.AST
import qualified Etch.Lexer as L

parse :: ByteString -> Either String [AST]
parse = parseOnly (many definitionParser)

definitionParser :: Parser AST
definitionParser = Definition
                <$> identifierParser
                <* L.charParser '='
                <*> primaryParser

primaryParser :: Parser AST
primaryParser = functionParser
             <|> identifierParser
             <|> integerLiteralParser

functionParser :: Parser AST
functionParser = Function
              <$ L.charParser '('
              <* L.charParser ')'
              <* L.charParser '{'
              <*> many definitionParser
              <* L.charParser '}'

identifierParser :: Parser AST
identifierParser = Identifier <$> L.identifierParser

integerLiteralParser :: Parser AST
integerLiteralParser = IntegerLiteral <$> L.integerParser
