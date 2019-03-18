module Etch.Analysis.Semantics where

import Data.Maybe (catMaybes)
import qualified Etch.Types.SyntaxTree as Syntax
import Etch.Types.SemanticTree

type Analysis = Either String

analysis :: [Syntax.Statement] -> Either String [Typed Statement]
analysis statements = catMaybes <$> traverse statementAnalysis statements

statementAnalysis :: Syntax.Statement -> Analysis (Maybe (Typed Statement))
statementAnalysis (Syntax.SigStatement _) = pure Nothing
statementAnalysis (Syntax.DefStatement expr) = Just . tymap DefStatement <$> defAnalysis expr
statementAnalysis (Syntax.ExprStatement expr) = Just . tymap ExprStatement <$> exprAnalysis expr

exprAnalysis :: Syntax.Expr -> Analysis (Typed Expr)
exprAnalysis (Syntax.CallExpr call) = tymap CallExpr <$> callAnalysis call
exprAnalysis (Syntax.BranchExpr branch) = tymap BranchExpr <$> branchAnalysis branch
exprAnalysis (Syntax.CompoundExpr compound) = tymap CompoundExpr <$> compoundAnalysis compound

compoundAnalysis :: Syntax.Compound -> Analysis (Typed Compound)
compoundAnalysis (Syntax.OpCompound op) = tymap OpCompound <$> opAnalysis op
compoundAnalysis (Syntax.PrimaryCompound primary) = tymap PrimaryCompound <$> primaryAnalysis primary

primaryAnalysis :: Syntax.Primary -> Analysis (Typed Primary)
primaryAnalysis (Syntax.BlockPrimary block) = tymap BlockPrimary <$> blockAnalysis block
primaryAnalysis (Syntax.TuplePrimary exprs) = do
    typeds <- traverse exprAnalysis exprs
    pure (TuplePrimary typeds `As` TupleType (fmap typedTy typeds))
primaryAnalysis (Syntax.IdentPrimary ident) = pure (IdentPrimary ident `As` IdentType)
primaryAnalysis (Syntax.IntegerPrimary x) = pure (IntegerPrimary x `As` IntType)
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
    pure (Op op l r `As` IntType)

blockAnalysis :: Syntax.Block -> Analysis (Typed Block)
blockAnalysis (Syntax.Block (Syntax.ParamList params) statements) = do
    s <- catMaybes <$> traverse statementAnalysis statements
    let retTy = if null s then UnitType else typedTy (last s)
    pure $ Block (ParamList args) s `As` FunctionType paramTys retTy
  where f (Syntax.Sig name ty) = name `As` fromSyntaxType ty
        args = f <$> params
        paramTys  = typedTy <$> args
