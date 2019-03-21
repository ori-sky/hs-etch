{-# LANGUAGE OverloadedStrings #-}

module Etch.Parser where

import qualified Data.Attoparsec.Text as Atto (parse)
import Data.Attoparsec.Text hiding (parse)
import Data.List (intercalate)
import Data.Text hiding (intercalate)
import Control.Applicative ((<|>), many)
import Control.Monad (when)
import qualified Etch.Lexer as L
import Etch.Types.SyntaxTree

parse :: Text -> Either String [Statement]
parse text = f (Atto.parse statementParser text)
  where f (Fail area contexts err)   = Left (unpack area ++ "\n" ++ err ++ "\n\n" ++ intercalate "\n" contexts)
        f (Partial cont)          = f (cont "")
        f (Done "" result)        = pure [result]
        f (Done remainder result) = (result :) <$> parse remainder

statementParser :: Parser Statement
statementParser = DefStatement  <$> defParser
              <|> ExprStatement <$> exprParser

exprParser :: Parser Expr
exprParser = CallExpr     <$> callParser
         <|> BranchExpr   <$> branchParser
         <|> CompoundExpr <$> compoundParser

compoundParser :: Parser Compound
compoundParser = OpCompound      <$> opParser
             <|> SigCompound     <$> sigParser primaryParser
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
typeParser = IntType 32 <$ L.charsParser "int"

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
