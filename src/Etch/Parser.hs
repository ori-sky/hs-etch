{-# LANGUAGE OverloadedStrings #-}

module Etch.Parser where

import Data.Attoparsec.Text hiding (parse)
import Data.Text
import Control.Applicative ((<|>), many)
import Control.Monad (when)
import qualified Etch.Lexer as L
import Etch.AST

parse :: Text -> Either String [Def]
parse text = parseOnly (many defParser) text

defParser :: Parser Def
defParser = Def <$> L.identifierParser <* L.charParser '=' <*> exprParser

exprParser :: Parser Expr
exprParser = BranchExpr   <$> branchParser
         <|> CompoundExpr <$> compoundParser

compoundParser :: Parser Compound
compoundParser = OpCompound      <$> opParser
             <|> PrimaryCompound <$> primaryParser

primaryParser :: Parser Primary
primaryParser = BlockPrimary   <$> blockParser
            <|> TuplePrimary   <$> tupleParser exprParser
            <|> IdentPrimary   <$> L.identifierParser
            <|> IntegerPrimary <$> L.integerParser
            <|> StringPrimary  <$> L.stringLiteralParser

branchParser :: Parser Branch
branchParser = Branch <$> compoundParser
                      <*  L.charParser '?'
                      <*> exprParser
                      <*  L.charParser ':'
                      <*> exprParser

opParser :: Parser Op
opParser = do
    lhs <- primaryParser
    op  <- L.operatorParser
    when (op == "->") (fail "operator `->` is reserved")
    Op op lhs <$> compoundParser

blockParser :: Parser Block
blockParser = Block
          <$> option [] (blockParamsParser <* L.charsParser "->")
          <*  L.charParser '{'
          <*> many exprParser
          <*  L.charParser '}'

blockParamsParser :: Parser [Text]
blockParamsParser = tupleParser L.identifierParser
                <|> pure <$> L.identifierParser

tupleParser :: Parser a -> Parser [a]
tupleParser p = L.charParser '('
             *> p `sepBy` L.charParser ','
             <* L.charParser ')'
