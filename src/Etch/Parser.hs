{-# LANGUAGE OverloadedStrings #-}

module Etch.Parser where

import Control.Applicative ((<|>), many)
import Data.ByteString.Char8
import Data.Attoparsec.ByteString.Char8 hiding (parse)
import Etch.AST
import qualified Etch.Lexer as L

parse :: ByteString -> Either String [AST]
parse = parseOnly (many exprParser)

exprParser :: Parser AST
exprParser = operatorParser <|> primaryParser

operatorParser :: Parser AST
operatorParser = do
        lhs <- primaryParser
        op <- L.operatorParser
        rhs <- exprParser
        pure (Call op (Tuple [lhs, rhs]))

primaryParser :: Parser AST
primaryParser = blockParser
             <|> tupleParser exprParser
             <|> identifierParser
             <|> integerLiteralParser
             <|> stringLiteralParser

blockParser :: Parser AST
blockParser = Block <$ L.charParser '{' <*> many exprParser <* L.charParser '}'

tupleParser :: Parser AST -> Parser AST
tupleParser elemP = Tuple <$ L.charParser '('
                          <*> elemP `sepBy` L.charParser ','
                          <* L.charParser ')'

identifierParser :: Parser AST
identifierParser = Identifier <$> L.identifierParser

integerLiteralParser :: Parser AST
integerLiteralParser = IntegerLiteral <$> L.integerParser

stringLiteralParser :: Parser AST
stringLiteralParser = StringLiteral <$> L.stringLiteralParser
