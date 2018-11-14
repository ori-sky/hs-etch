{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Etch.CodeGen where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as ShortBS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (join, when)
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

codeGen :: Module -> IO String
codeGen m = L.withContext $ \ctx ->
    L.withModuleFromAST ctx lAST (fmap BS.unpack . L.moduleLLVMAssembly)
  where srcFile = (ShortBS.toShort . encodeUtf8 . T.pack . moduleSourceFile) m
        lAST = (moduleGen m) { L.AST.moduleSourceFileName = srcFile }

moduleGen :: Module -> L.AST.Module
moduleGen m = IR.buildModule name $ do
    traverse_ definitionBuilder (moduleDefinitions m)
  where name = (ShortBS.toShort . encodeUtf8 . T.pack . moduleName) m

definitionBuilder :: IR.MonadModuleBuilder m => Def -> m L.AST.Operand
definitionBuilder (Def name (CompoundExpr (PrimaryCompound (BlockPrimary (Block params exprs))))) =
    IR.function lName lArgs L.AST.i32 $ \argOperands -> do
        let args = HM.fromList (zip params argOperands)
        results <- traverse (exprBuilder [args]) exprs
        when (not (null results)) (IR.ret (last results))
  where lName = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
        lArgs = (L.AST.i32,) . IR.ParameterName . ShortBS.toShort . encodeUtf8 <$> params
definitionBuilder (Def name (CompoundExpr (PrimaryCompound (IntegerPrimary x)))) =
    IR.global lName L.AST.i32 constant
  where lName = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
        constant = L.AST.Const.Int 32 x
definitionBuilder def = error (show def)

exprBuilder :: IR.MonadIRBuilder m => [Scope] -> Expr -> m L.AST.Operand
exprBuilder scopes (BranchExpr branch) = branchBuilder scopes branch
exprBuilder scopes (CompoundExpr compound) = compoundBuilder scopes compound

compoundBuilder :: IR.MonadIRBuilder m => [Scope] -> Compound -> m L.AST.Operand
compoundBuilder scopes (OpCompound op) = opBuilder scopes op
compoundBuilder scopes (PrimaryCompound primary) = primaryBuilder scopes primary

primaryBuilder :: IR.MonadIRBuilder m => [Scope] -> Primary -> m L.AST.Operand
primaryBuilder scopes (TuplePrimary [expr]) = exprBuilder scopes expr
primaryBuilder (scope:_) (IdentPrimary name) = case HM.lookup name scope of
    Just operand -> pure operand
    Nothing      -> error ("undefined name: " ++ T.unpack name)
primaryBuilder _ (IntegerPrimary x) = IR.int32 x
primaryBuilder _ primary = error (show primary)

branchBuilder :: IR.MonadIRBuilder m => [Scope] -> Branch -> m L.AST.Operand
branchBuilder scopes (Branch cond trueBranch falseBranch) =
    join (IR.select <$> compoundBuilder scopes cond
                    <*> exprBuilder scopes trueBranch
                    <*> exprBuilder scopes falseBranch)

opBuilder :: IR.MonadIRBuilder m => [Scope] -> Op -> m L.AST.Operand
opBuilder scopes (Op op lhs rhs) = do
    l <- primaryBuilder scopes lhs
    r <- compoundBuilder scopes rhs
    case op of
        "+"  -> IR.add l r
        "-"  -> IR.sub l r
        "*"  -> IR.mul l r
        "==" -> IR.icmp L.AST.EQ l r
        "<"  -> IR.icmp L.AST.SLT l r
        o   -> error (show o)
