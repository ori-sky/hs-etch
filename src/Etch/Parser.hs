{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Etch.Parser where

import qualified Data.Attoparsec.Text as Atto (parse)
import Data.Attoparsec.Text hiding (parse)
import Data.Text
import Control.Applicative ((<|>), many)
import Control.Monad.Except
import qualified Etch.Lexer as L
import Etch.Types.ErrorContext
import Etch.Types.SyntaxTree

parse :: MonadError ErrorContext m => Text -> m [Statement]
parse text = f (Atto.parse statementParser text)
  where f (Fail area contexts err) = throwError $ ErrorContext ("parser failure: " ++ err) (unpack area : contexts)
        f (Partial cont)           = f (cont "")
        f (Done "" result)         = pure [result]
        f (Done remainder result)  = (result :) <$> parse remainder

statementParser :: Parser Statement
statementParser = DefStatement  <$> defParser
              <|> ExprStatement <$> exprParser

exprParser :: Parser Expr
exprParser = FunctionExpr <$> functionParser
         <|> CallExpr     <$> callParser
         <|> BranchExpr   <$> branchParser
         <|> CompoundExpr <$> compoundParser

compoundParser :: Parser Compound
compoundParser = OpCompound   <$> opParser
             <|> AtomCompound <$> atomParser

atomParser :: Parser Atom
atomParser = SigAtom     <$> sigParser primaryParser
         <|> PrimaryAtom <$> primaryParser

primaryParser :: Parser Primary
primaryParser = BlockPrimary   <$> blockParser
            <|> TuplePrimary   <$> tupleParser exprParser '(' ',' ')'
            <|> NewPrimary     <$> tupleParser exprParser '<' ',' '>'
            <|> IdentPrimary   <$> L.identifierParser
            <|> IntegerPrimary <$> L.integerParser
            <|> StringPrimary  <$> L.stringLiteralParser

sigParser :: Parser a -> Parser (Sig a)
sigParser p = Sig <$> p <* L.charParser ':' <*> atomParser

defParser :: Parser Def
defParser = Def <$> L.identifierParser <* L.charParser '=' <*> exprParser

functionParser :: Parser Function
functionParser = Function <$> paramListParser <* L.charsParser "->" <*> exprParser

callParser :: Parser Call
callParser = Call <$> compoundParser <* L.charsParser "<-" <*> exprParser

branchParser :: Parser Branch
branchParser = Branch <$> compoundParser
                      <*  L.charParser '?'
                      <*> exprParser
                      <*> exprParser

opParser :: Parser Op
opParser = do
    lhs <- atomParser
    op  <- L.operatorParser
    when (op == "->") (fail "operator `->` is reserved")
    when (op == "<-") (fail "operator `<-` is reserved")
    Op op lhs <$> compoundParser

blockParser :: Parser Block
blockParser = do
    L.charParser '{'
    Block <$> many statementParser <* L.charParser '}'

paramListParser :: Parser ParamList
paramListParser = ParamList <$> tupleParser paramParser '(' ',' ')'
              <|> ParamList <$> pure <$> paramParser

paramParser :: Parser Param
paramParser = SigParam      <$> sigParser L.identifierParser
          <|> InferredParam <$> L.identifierParser

tupleParser :: Parser a -> Char -> Char -> Char -> Parser [a]
tupleParser p start sep end = L.charParser start
                           *> p `sepBy` L.charParser sep
                           <* L.charParser end
