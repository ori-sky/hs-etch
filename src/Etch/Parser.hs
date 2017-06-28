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
             <|> integerLiteralParser
             <|> identifierParser

blockParser :: Parser AST
blockParser = Block <$ L.charParser '{' <*> many operatorParser <* L.charParser '}'

tupleParser :: Parser AST
tupleParser = Tuple <$ L.charParser '('
                    <*> operatorParser `sepBy` L.charParser ','
                    <* L.charParser ')'

integerLiteralParser :: Parser AST
integerLiteralParser = IntegerLiteral <$> L.integerParser

identifierParser :: Parser AST
identifierParser = Identifier <$> L.identifierParser
