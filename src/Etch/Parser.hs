{-# LANGUAGE OverloadedStrings #-}

module Etch.Parser where

import Data.Attoparsec.Text hiding (parse)
import Data.Text
import Control.Applicative ((<|>), many)
import Control.Monad (when)
import qualified Etch.Lexer as L
import Etch.Types.SyntaxTree

parse :: Text -> Either String [Statement]
parse text = parseOnly (many statementParser) text

statementParser :: Parser Statement
statementParser = SigStatement  <$> sigParser exprParser
              <|> DefStatement  <$> defParser
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

sigParser :: Parser a -> Parser (Sig a)
sigParser p = Sig <$> p <* L.charParser ':' <*> typeParser

typeParser :: Parser Type
typeParser = IntType <$ L.charsParser "int"

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
blockParser = Block <$> paramListParser <* L.charsParser "->" <*> blockInnerParser
          <|> Block (ParamList [])                            <$> blockInnerParser

blockInnerParser :: Parser [Statement]
blockInnerParser = L.charParser '{'
                *> many statementParser
               <*  L.charParser '}'

paramListParser :: Parser ParamList
paramListParser = ParamList <$> tupleParser paramParser
              <|> ParamList <$> pure <$> paramParser

paramParser :: Parser (Sig Text)
paramParser = sigParser L.identifierParser
          <|> Sig <$> L.identifierParser <*> pure InferredType

tupleParser :: Parser a -> Parser [a]
tupleParser p = L.charParser '('
             *> p `sepBy` L.charParser ','
             <* L.charParser ')'
