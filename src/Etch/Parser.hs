{-# LANGUAGE OverloadedStrings #-}

module Etch.Parser where

import Control.Applicative ((<|>), many)
import Data.ByteString.Char8
import Data.Attoparsec.ByteString.Char8 hiding (parse)
import Etch.AST
import qualified Etch.Lexer as L

parse :: ByteString -> Either String [AST]
parse = parseOnly (many operatorParser)

operatorParser :: Parser AST
operatorParser = do
        lhs <- primaryParser
        op <- L.operatorParser
        rhs <- operatorParser
        pure (Call op (Tuple [lhs, rhs]))
    <|> primaryParser

primaryParser :: Parser AST
primaryParser = blockParser
             <|> tupleParser
             <|> identifierParser
             <|> integerLiteralParser
             <|> stringLiteralParser

blockParser :: Parser AST
blockParser = Block <$ L.charParser '{' <*> many operatorParser <* L.charParser '}'

tupleParser :: Parser AST
tupleParser = Tuple <$ L.charParser '('
                    <*> operatorParser `sepBy` L.charParser ','
                    <* L.charParser ')'

identifierParser :: Parser AST
identifierParser = Identifier <$> L.identifierParser

integerLiteralParser :: Parser AST
integerLiteralParser = IntegerLiteral <$> L.integerParser

stringLiteralParser :: Parser AST
stringLiteralParser = StringLiteral <$> L.stringLiteralParser
