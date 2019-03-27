{-# LANGUAGE LambdaCase #-}

module Etch.Analysis.Semantics where

import Text.Show.Pretty (ppShow)
import qualified Etch.Types.SyntaxTree as Syntax
import Etch.Types.ErrorContext
import Etch.Types.SemanticTree

type Analysis = Either ErrorContext

analysis :: [Syntax.Statement] -> Analysis [Typed Statement]
analysis statements = traverse statementAnalysis statements

statementAnalysis :: Syntax.Statement -> Analysis (Typed Statement)
statementAnalysis (Syntax.DefStatement expr)  = tymap DefStatement  <$> defAnalysis expr
statementAnalysis (Syntax.ExprStatement expr) = tymap ExprStatement <$> exprAnalysis expr

exprAnalysis :: Syntax.Expr -> Analysis (Typed Expr)
exprAnalysis (Syntax.CallExpr call)         = tymap CallExpr     <$> callAnalysis call
exprAnalysis (Syntax.BranchExpr branch)     = tymap BranchExpr   <$> branchAnalysis branch
exprAnalysis (Syntax.CompoundExpr compound) = tymap CompoundExpr <$> compoundAnalysis compound

compoundAnalysis :: Syntax.Compound -> Analysis (Typed Compound)
compoundAnalysis (Syntax.OpCompound op)     = tymap OpCompound      <$> opAnalysis op
compoundAnalysis (Syntax.AtomCompound atom) = tymap PrimaryCompound <$> atomAnalysis atom

atomAnalysis :: Syntax.Atom -> Analysis (Typed Primary)
atomAnalysis (Syntax.SigAtom sig@(Syntax.Sig primary atom)) = do
    pVal `As` actualTy <- primaryAnalysis primary
    atomAnalysis atom >>= \case
        TypePrimary (expectedTy `As` _) `As` _ -> if actualTy == expectedTy || actualTy == UnresolvedType
            then pure (pVal `As` expectedTy)
            else Left $ ErrorContext "expected type does not match actual type" [ppShow expectedTy, ppShow actualTy, show sig]
        t@(_ `As` UnresolvedType)              -> pure (pVal `As` UnresolvedPrimaryType t)
        typed                                  -> Left $ ErrorContext "not a type" [ppShow typed]
atomAnalysis (Syntax.PrimaryAtom primary) = primaryAnalysis primary

primaryAnalysis :: Syntax.Primary -> Analysis (Typed Primary)
primaryAnalysis (Syntax.BlockPrimary block) = tymap BlockPrimary <$> blockAnalysis block
primaryAnalysis (Syntax.TuplePrimary exprs) = do
    typeds <- traverse exprAnalysis exprs
    pure (TuplePrimary typeds `As` TupleType (typedTy <$> typeds))
primaryAnalysis (Syntax.IdentPrimary ident) = pure (IdentPrimary ident `As` UnresolvedType)
primaryAnalysis (Syntax.IntegerPrimary x)   = pure (IntegerPrimary x   `As` IntType 32)
primaryAnalysis (Syntax.StringPrimary s)    = pure (StringPrimary s    `As` StringType)

defAnalysis :: Syntax.Def -> Analysis (Typed Def)
defAnalysis (Syntax.Def name expr) = tymap (Def name) <$> exprAnalysis expr

callAnalysis :: Syntax.Call -> Analysis (Typed Call)
callAnalysis (Syntax.Call callable expr) = do
    c <- compoundAnalysis callable
    e <- exprAnalysis expr
    case typedTy c of
        FunctionType _ retTy -> pure (Call c e `As` retTy)
        _                    -> Left $ ErrorContext "compound is not callable" [ppShow c]

branchAnalysis :: Syntax.Branch -> Analysis (Typed Branch)
branchAnalysis (Syntax.Branch cond trueBranch falseBranch) = do
    c <- compoundAnalysis cond
    t <- exprAnalysis trueBranch
    f <- exprAnalysis falseBranch
    pure (Branch c t f `As` typedTy t)

opAnalysis :: Syntax.Op -> Analysis (Typed Op)
opAnalysis (Syntax.Op op lhs rhs) = do
    l <- atomAnalysis lhs
    r <- compoundAnalysis rhs
    pure (Op op l r `As` typedTy l)

blockAnalysis :: Syntax.Block -> Analysis (Typed Block)
blockAnalysis (Syntax.Block (Syntax.ParamList params) statements) = do
    args <- traverse paramAnalysis params
    s <- traverse statementAnalysis statements
    let retTy = if null s then TupleType [] else typedTy (last s)
        paramTys = typedTy <$> args
    pure $ Block (ParamList args) s `As` FunctionType paramTys retTy

paramAnalysis :: Syntax.Param -> Analysis (Typed Param)
paramAnalysis (Syntax.SigParam (Syntax.Sig name atom)) = (name `As`) . typedTy <$> atomAnalysis atom
-- paramAnalysis (Syntax.SigParam (Syntax.AtomSig name ty)) = atomAnalysis >>= \case
--     TypePrimary t `As` _ ->

--        TypePrimary (expectedTy `As` _) `As` _ -> if actualTy == expectedTy || actualTy == UnresolvedType
--            then pure (pVal `As` expectedTy)
--            else fail ("expected type `" ++ show expectedTy ++ "` does not match actual type `" ++ show actualTy ++ "`: " ++ show sig)
paramAnalysis (Syntax.InferredParam name)            = pure (name `As` UnresolvedType)
