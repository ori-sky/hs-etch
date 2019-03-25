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
statementAnalysis (Syntax.DefStatement expr) = tymap DefStatement <$> defAnalysis expr
statementAnalysis (Syntax.ExprStatement expr) = tymap ExprStatement <$> exprAnalysis expr

exprAnalysis :: Syntax.Expr -> Analysis (Typed Expr)
exprAnalysis (Syntax.CallExpr call) = tymap CallExpr <$> callAnalysis call
exprAnalysis (Syntax.BranchExpr branch) = tymap BranchExpr <$> branchAnalysis branch
exprAnalysis (Syntax.CompoundExpr compound) = tymap CompoundExpr <$> compoundAnalysis compound

compoundAnalysis :: Syntax.Compound -> Analysis (Typed Compound)
compoundAnalysis (Syntax.OpCompound op) = tymap OpCompound <$> opAnalysis op
compoundAnalysis (Syntax.AtomCompound atom) = tymap PrimaryCompound <$> atomAnalysis atom

atomAnalysis :: Syntax.Atom -> Analysis (Typed Primary)
atomAnalysis (Syntax.SigAtom sig@(Syntax.Sig primary ty)) = do
    expectedTy `As` _ <- typeAnalysis ty
    pVal `As` actualTy <- primaryAnalysis primary
    if actualTy == expectedTy || actualTy == UnresolvedType
        then pure (pVal `As` expectedTy)
        else Left $ ErrorContext "expected type does not match actual type" [ppShow expectedTy, ppShow actualTy, show sig]
atomAnalysis (Syntax.SigAtom sig@(Syntax.AtomSig primary ty)) = do
    pVal `As` actualTy <- primaryAnalysis primary
    atomAnalysis ty >>= \case
        TypePrimary (expectedTy `As` _) `As` _ -> if actualTy == expectedTy || actualTy == UnresolvedType
            then pure (pVal `As` expectedTy)
            else Left $ ErrorContext "expected type does not match actual type" [ppShow expectedTy, ppShow actualTy, show sig]
        t@(_ `As` UnresolvedType)              -> pure (pVal `As` UnresolvedPrimaryType t)
        typed                                  -> Left $ ErrorContext "not a type" [ppShow typed]
atomAnalysis (Syntax.PrimaryAtom primary) = primaryAnalysis primary

primaryAnalysis :: Syntax.Primary -> Analysis (Typed Primary)
primaryAnalysis (Syntax.BlockPrimary block) = tymap BlockPrimary <$> blockAnalysis block
primaryAnalysis (Syntax.TypePrimary ty) = tymap TypePrimary <$> typeAnalysis ty
primaryAnalysis (Syntax.TuplePrimary exprs) = do
    typeds <- traverse exprAnalysis exprs
    pure (TuplePrimary typeds `As` TupleType (fmap typedTy typeds))
primaryAnalysis (Syntax.IdentPrimary ident) = pure (IdentPrimary ident `As` UnresolvedType)
primaryAnalysis (Syntax.IntegerPrimary x) = pure (IntegerPrimary x `As` IntType 32)
primaryAnalysis (Syntax.StringPrimary s) = pure (StringPrimary s `As` StringType)

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
    let retTy = if null s then UnitType else typedTy (last s)
        paramTys = typedTy <$> args
    pure $ Block (ParamList args) s `As` FunctionType paramTys retTy

typeAnalysis :: Syntax.Type -> Analysis (Typed Type)
typeAnalysis (Syntax.IntType bits) = pure (IntType bits `As` TypeType)
typeAnalysis (Syntax.NewType (Syntax.ParamList params) primaries) = do
    args <- traverse paramAnalysis params
    p <- traverse atomAnalysis primaries
    pure $ NewType (ParamList args) p `As` TypeType

paramAnalysis :: Syntax.Param -> Analysis (Typed Param)
paramAnalysis (Syntax.SigParam (Syntax.Sig name ty))     = (name `As`) . typedTy <$> typeAnalysis ty
-- paramAnalysis (Syntax.SigParam (Syntax.AtomSig name ty)) = atomAnalysis >>= \case
--     TypePrimary t `As` _ ->

--        TypePrimary (expectedTy `As` _) `As` _ -> if actualTy == expectedTy || actualTy == UnresolvedType
--            then pure (pVal `As` expectedTy)
--            else fail ("expected type `" ++ show expectedTy ++ "` does not match actual type `" ++ show actualTy ++ "`: " ++ show sig)
paramAnalysis (Syntax.InferredParam name)            = pure (name `As` UnresolvedType)
paramAnalysis param = Left $ ErrorContext "unhandled param" [show param]
