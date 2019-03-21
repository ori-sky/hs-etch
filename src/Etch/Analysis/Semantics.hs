module Etch.Analysis.Semantics where

import qualified Etch.Types.SyntaxTree as Syntax
import Etch.Types.SemanticTree

type Analysis = Either String

analysis :: [Syntax.Statement] -> Either String [Typed Statement]
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
compoundAnalysis (Syntax.SigCompound sig@(Syntax.Sig primary ty)) = do
    expectedTy `As` _ <- typeAnalysis ty
    e@(_ `As` actualTy) <- primaryAnalysis primary
    if actualTy == expectedTy || actualTy == UnresolvedType
        then pure (PrimaryCompound e `As` expectedTy)
        else fail ("expected type `" ++ show expectedTy ++ "` does not match actual type `" ++ show actualTy ++ "`: " ++ show sig)
compoundAnalysis (Syntax.PrimaryCompound primary) = tymap PrimaryCompound <$> primaryAnalysis primary

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
        _                    -> fail ("compound is not callable: " ++ show c)

branchAnalysis :: Syntax.Branch -> Analysis (Typed Branch)
branchAnalysis (Syntax.Branch cond trueBranch falseBranch) = do
    c <- compoundAnalysis cond
    t <- exprAnalysis trueBranch
    f <- exprAnalysis falseBranch
    pure (Branch c t f `As` typedTy t)

opAnalysis :: Syntax.Op -> Analysis (Typed Op)
opAnalysis (Syntax.Op op lhs rhs) = do
    l <- primaryAnalysis lhs
    r <- compoundAnalysis rhs
    pure (Op op l r `As` typedTy l)

blockAnalysis :: Syntax.Block -> Analysis (Typed Block)
blockAnalysis (Syntax.Block (Syntax.ParamList params) statements) = do
    args <- traverse paramAnalysis params
    s <- traverse statementAnalysis statements
    let retTy = if null s then UnitType else typedTy (last s)
        paramTys = typedTy <$> args
    pure $ Block (ParamList args) s `As` FunctionType paramTys retTy

paramAnalysis :: Syntax.Param -> Analysis (Typed Param)
paramAnalysis (Syntax.SigParam (Syntax.Sig name ty)) = (name `As`) . typedTy <$> typeAnalysis ty
paramAnalysis (Syntax.InferredParam name)            = pure (name `As` UnresolvedType)

typeAnalysis :: Syntax.Type -> Analysis (Typed Type)
typeAnalysis (Syntax.IntType bits) = pure (IntType bits `As` KindType TypeKind)
typeAnalysis (Syntax.NewType exprs) = do
    e <- traverse exprAnalysis exprs
    pure (NewType e `As` KindType TypeKind)
