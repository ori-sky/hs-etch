{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Etch.CodeGen where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as ShortBS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Foldable (traverse_)
import Data.Maybe (maybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (join)
import Control.Monad.State
import qualified LLVM              as L
import qualified LLVM.Context      as L
import qualified LLVM.AST          as L.AST hiding (type')
import qualified LLVM.AST.Constant as L.AST.Const
--import qualified LLVM.AST.Global   as L.AST
import qualified LLVM.AST.IntegerPredicate as L.AST
import qualified LLVM.AST.Type     as L.AST (i32)
import qualified LLVM.IRBuilder.Constant    as IR
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module      as IR
import qualified LLVM.IRBuilder.Monad       as IR
import Etch.AST
import Etch.Module

type Scope = HM.HashMap Text L.AST.Operand
type Builder a = forall m. IR.MonadModuleBuilder m => StateT [Scope] (IR.IRBuilderT m) a

codeGen :: Module -> IO String
codeGen m = L.withContext $ \ctx ->
    L.withModuleFromAST ctx lAST (fmap BS.unpack . L.moduleLLVMAssembly)
  where srcFile = (ShortBS.toShort . encodeUtf8 . T.pack . moduleSourceFile) m
        lAST = (moduleGen m) { L.AST.moduleSourceFileName = srcFile }

moduleGen :: Module -> L.AST.Module
moduleGen m = IR.buildModule name $ do
    traverse_ topLevelDefBuilder (moduleDefs m)
  where name = (ShortBS.toShort . encodeUtf8 . T.pack . moduleName) m

topLevelDefBuilder :: IR.MonadModuleBuilder m => Def -> m L.AST.Operand
topLevelDefBuilder (Def name (CompoundExpr (PrimaryCompound (BlockPrimary (Block params exprs))))) =
    IR.function lName lArgs L.AST.i32 $ \argOperands -> do
        let args = HM.fromList (zip params argOperands)
        results <- evalStateT (traverse statementBuilder exprs) [args]
        when (not (null results)) (IR.ret (last results))
  where lName = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
        lArgs = (L.AST.i32,) . IR.ParameterName . ShortBS.toShort . encodeUtf8 <$> params
topLevelDefBuilder (Def name (CompoundExpr (PrimaryCompound (IntegerPrimary x)))) =
    IR.global lName L.AST.i32 constant
  where lName = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
        constant = L.AST.Const.Int 32 x
topLevelDefBuilder def = error (show def)

statementBuilder :: Statement -> Builder L.AST.Operand
statementBuilder (DefStatement def) = defBuilder def
statementBuilder (ExprStatement expr) = exprBuilder expr

exprBuilder :: Expr -> Builder L.AST.Operand
exprBuilder (BranchExpr branch) = branchBuilder branch
exprBuilder (CompoundExpr compound) = compoundBuilder compound

compoundBuilder :: Compound -> Builder L.AST.Operand
compoundBuilder (OpCompound op) = opBuilder op
compoundBuilder (PrimaryCompound primary) = primaryBuilder primary

primaryBuilder :: Primary -> Builder L.AST.Operand
primaryBuilder (TuplePrimary [expr]) = exprBuilder expr
primaryBuilder (IdentPrimary name) = get >>= f
  where f (scope:xs) = maybe (f xs) pure (HM.lookup name scope)
        f [] = error ("undefined name: " ++ T.unpack name)
primaryBuilder (IntegerPrimary x) = IR.int32 x
primaryBuilder primary = error (show primary)

defBuilder :: Def -> Builder L.AST.Operand
defBuilder (Def name expr) = do
    e <- exprBuilder expr
    modify $ \(scope:xs) -> HM.insert name e scope : xs
    pure e

branchBuilder :: Branch -> Builder L.AST.Operand
branchBuilder (Branch cond trueBranch falseBranch) =
    join (IR.select <$> compoundBuilder cond
                    <*> exprBuilder trueBranch
                    <*> exprBuilder falseBranch)

opBuilder :: Op -> Builder L.AST.Operand
opBuilder (Op op lhs rhs) = do
    l <- primaryBuilder lhs
    r <- compoundBuilder rhs
    case op of
        "+"  -> IR.add l r
        "-"  -> IR.sub l r
        "*"  -> IR.mul l r
        "==" -> IR.icmp L.AST.EQ l r
        "<"  -> IR.icmp L.AST.SLT l r
        o   -> error (show o)
