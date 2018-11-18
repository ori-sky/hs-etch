{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Etch.CodeGen where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as ShortBS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Foldable (traverse_)
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
import Etch.CodeGen.Context
import Etch.Types.Module
import Etch.Types.SemanticTree

type ModuleBuilder = IR.ModuleBuilderT (State (Context L.AST.Operand))
type Builder = IR.IRBuilderT ModuleBuilder

-- thanks to Solonarv from #haskell for this gem
locally :: MonadState s m => m a -> m a
locally action = (action <*) . put =<< get

codeGen :: Module -> IO String
codeGen m = L.withContext $ \ctx ->
    L.withModuleFromAST ctx lAST (fmap BS.unpack . L.moduleLLVMAssembly)
  where srcFile = (ShortBS.toShort . encodeUtf8 . T.pack . moduleSourceFile) m
        lAST = (moduleGen m) { L.AST.moduleSourceFileName = srcFile }

moduleGen :: Module -> L.AST.Module
moduleGen m = evalState (IR.buildModuleT name moduleBuilder) defaultContext
  where name = (ShortBS.toShort . encodeUtf8 . T.pack . moduleName) m
        moduleBuilder = traverse_ topLevelDefBuilder (moduleDefs m)

topLevelDefBuilder :: Def -> ModuleBuilder L.AST.Operand
topLevelDefBuilder (Def name (CompoundExpr (PrimaryCompound (BlockPrimary block)))) = do
    fn <- functionBuilder lName block
    modify (contextInsert name fn)
    pure fn
  where lName = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
topLevelDefBuilder (Def name (CompoundExpr (PrimaryCompound (IntegerPrimary x)))) = do
    g <- IR.global lName L.AST.i32 constant
    modify (contextInsert name constantOp)
    pure g
  where lName = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
        constant = L.AST.Const.Int 32 x
        constantOp = L.AST.ConstantOperand constant
topLevelDefBuilder def = error (show def)

statementBuilder :: Statement -> Builder L.AST.Operand
statementBuilder (DefStatement def) = defBuilder def
statementBuilder (ExprStatement expr) = exprBuilder expr

exprBuilder :: Expr -> Builder L.AST.Operand
exprBuilder (CallExpr call) = callBuilder call
exprBuilder (BranchExpr branch) = branchBuilder branch
exprBuilder (CompoundExpr compound) = compoundBuilder compound

compoundBuilder :: Compound -> Builder L.AST.Operand
compoundBuilder (OpCompound op) = opBuilder op
compoundBuilder (PrimaryCompound primary) = primaryBuilder primary

primaryBuilder :: Primary -> Builder L.AST.Operand
primaryBuilder (BlockPrimary block) = blockBuilder block
primaryBuilder (TuplePrimary [expr]) = exprBuilder expr
primaryBuilder (IdentPrimary name) = get >>= f
  where f context = case contextLookup name context of
            Just value -> pure value
            Nothing    -> error ("undefined name " ++ T.unpack name)
primaryBuilder (IntegerPrimary x) = IR.int32 x
primaryBuilder primary = error (show primary)

defBuilder :: Def -> Builder L.AST.Operand
defBuilder (Def name expr) = do
    e <- exprBuilder expr
    modify (contextInsert name e)
    pure e

callBuilder :: Call -> Builder L.AST.Operand
callBuilder (Call callable (CompoundExpr (PrimaryCompound (TuplePrimary args)))) = do
    fn <- compoundBuilder callable
    as <- traverse exprBuilder args
    IR.call fn (fmap (, []) as)
callBuilder (Call callable (CompoundExpr (PrimaryCompound primary@(IntegerPrimary _)))) = do
    fn <- compoundBuilder callable
    p <- primaryBuilder primary
    IR.call fn [(p, [])]
callBuilder call = error (show call)

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

blockBuilder :: Block -> Builder L.AST.Operand
blockBuilder block = do
    nextID <- fromInteger <$> contextStateNextID
    lift $ functionBuilder (L.AST.UnName nextID) block

functionBuilder :: L.AST.Name -> Block -> ModuleBuilder L.AST.Operand
functionBuilder lName (Block args statements) =
    IR.function lName lArgs L.AST.i32 $ \argOperands -> do
        results <- locally $ do
            modify $ contextInsertScope (HM.fromList (zip args argOperands))
            traverse statementBuilder statements
        when (not (null results)) (IR.ret (last results))
  where lArgs = (L.AST.i32,) . IR.ParameterName . ShortBS.toShort . encodeUtf8 <$> args
