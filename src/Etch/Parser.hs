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

statementParser :: Parser Statement
statementParser = DefStatement  <$> defParser
              <|> ExprStatement <$> exprParser

exprParser :: Parser Expr
exprParser = CallExpr     <$> callParser
         <|> BranchExpr   <$> branchParser
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

defParser :: Parser Def
defParser = Def <$> L.identifierParser <* L.charParser '=' <*> exprParser

callParser :: Parser Call
callParser = Call <$> compoundParser <* L.charsParser "<-" <*> exprParser

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
    when (op == "<-") (fail "operator `<-` is reserved")
    Op op lhs <$> compoundParser

blockParser :: Parser Block
blockParser = Block
          <$> option [] (blockParamsParser <* L.charsParser "->")
          <*  L.charParser '{'
          <*> many statementParser
          <*  L.charParser '}'

blockParamsParser :: Parser [Text]
blockParamsParser = tupleParser L.identifierParser
                <|> pure <$> L.identifierParser

tupleParser :: Parser a -> Parser [a]
tupleParser p = L.charParser '('
             *> p `sepBy` L.charParser ','
             <* L.charParser ')'
