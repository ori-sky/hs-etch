module Etch.Analysis.Semantics where

import Data.Maybe (catMaybes)
import qualified Etch.Types.SyntaxTree as Syntax
import qualified Etch.Types.SemanticTree as Semantic

type Analysis = Either String

analysis :: [Syntax.Statement] -> Either String [Semantic.Statement]
analysis statements = catMaybes <$> traverse statementAnalysis statements

statementAnalysis :: Syntax.Statement -> Analysis (Maybe Semantic.Statement)
statementAnalysis (Syntax.SigStatement _) = pure Nothing
statementAnalysis (Syntax.DefStatement def) = pure . Semantic.DefStatement <$> defAnalysis def
statementAnalysis (Syntax.ExprStatement expr) = pure . Semantic.ExprStatement <$> exprAnalysis expr

exprAnalysis :: Syntax.Expr -> Analysis Semantic.Expr
exprAnalysis (Syntax.CallExpr call) = Semantic.CallExpr <$> callAnalysis call
exprAnalysis (Syntax.BranchExpr branch) = Semantic.BranchExpr <$> branchAnalysis branch
exprAnalysis (Syntax.CompoundExpr compound) = Semantic.CompoundExpr <$> compoundAnalysis compound

compoundAnalysis :: Syntax.Compound -> Analysis Semantic.Compound
compoundAnalysis (Syntax.OpCompound op) = Semantic.OpCompound <$> opAnalysis op
compoundAnalysis (Syntax.PrimaryCompound primary) = Semantic.PrimaryCompound <$> primaryAnalysis primary

primaryAnalysis :: Syntax.Primary -> Analysis Semantic.Primary
primaryAnalysis (Syntax.BlockPrimary block) = Semantic.BlockPrimary <$> blockAnalysis block
primaryAnalysis (Syntax.TuplePrimary exprs) = Semantic.TuplePrimary <$> traverse exprAnalysis exprs
primaryAnalysis (Syntax.IdentPrimary ident) = pure (Semantic.IdentPrimary ident)
primaryAnalysis (Syntax.IntegerPrimary x) = pure (Semantic.IntegerPrimary x)
primaryAnalysis (Syntax.StringPrimary s) = pure (Semantic.StringPrimary s)

defAnalysis :: Syntax.Def -> Analysis Semantic.Def
defAnalysis (Syntax.Def name expr) = Semantic.Def name <$> exprAnalysis expr

callAnalysis :: Syntax.Call -> Analysis Semantic.Call
callAnalysis (Syntax.Call callable expr) = Semantic.Call <$> compoundAnalysis callable <*> exprAnalysis expr

branchAnalysis :: Syntax.Branch -> Analysis Semantic.Branch
branchAnalysis (Syntax.Branch cond trueBranch falseBranch) = Semantic.Branch <$> compoundAnalysis cond <*> exprAnalysis trueBranch <*> exprAnalysis falseBranch

opAnalysis :: Syntax.Op -> Analysis Semantic.Op
opAnalysis (Syntax.Op op lhs rhs) = Semantic.Op op <$> primaryAnalysis lhs <*> compoundAnalysis rhs

blockAnalysis :: Syntax.Block -> Analysis Semantic.Block
blockAnalysis (Syntax.Block args statements) = Semantic.Block args . catMaybes <$> traverse statementAnalysis statements
