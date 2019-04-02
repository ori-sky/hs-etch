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
import Text.Show.Pretty (ppShow)
import Control.Monad (join)
import Control.Monad.State
import qualified LLVM                       as L
import qualified LLVM.Context               as L
import qualified LLVM.AST                   as L.AST hiding (type')
import qualified LLVM.AST.Constant          as L.AST.Const
--import qualified LLVM.AST.Global            as L.AST
import qualified LLVM.AST.IntegerPredicate  as L.AST
import qualified LLVM.AST.Type              as L.AST (void, i32)
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
        lAST    = (moduleGen m) { L.AST.moduleSourceFileName = srcFile }

moduleGen :: Module -> L.AST.Module
moduleGen m = evalState (IR.buildModuleT name moduleBuilder) defaultContext
  where name          = (ShortBS.toShort . encodeUtf8 . T.pack . moduleName) m
        moduleBuilder = traverse_ topLevelDefBuilder (moduleDefs m)

topLevelDefBuilder :: Typed Def -> ModuleBuilder (Maybe L.AST.Operand)
topLevelDefBuilder (Def name (CompoundExpr (PrimaryCompound (BlockPrimary block `As` _) `As` _) `As` _) `As` _) = do
    fn <- functionBuilder lName block
    modify (contextInsert name fn)
    pure (Just fn)
  where lName = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
topLevelDefBuilder (Def name (CompoundExpr (PrimaryCompound (IntegerPrimary x `As` _) `As` _) `As` _) `As` _) = do
    modify (contextInsert name constantOp)
    Just <$> IR.global lName L.AST.i32 constant
  where lName      = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
        constant   = L.AST.Const.Int 32 x
        constantOp = L.AST.ConstantOperand constant
topLevelDefBuilder (_ `As` NewType _ _) = pure Nothing
topLevelDefBuilder (_ `As` UnresolvedPrimaryType primary) = error ("unresolved primary type:\n\n" ++ ppShow primary)
topLevelDefBuilder def = error ("unhandled top-level def:\n\n" ++ ppShow def)

statementBuilder :: Typed Statement -> Builder L.AST.Operand
statementBuilder (DefStatement def   `As` _) = defBuilder def
statementBuilder (ExprStatement expr `As` _) = exprBuilder expr

exprBuilder :: Typed Expr -> Builder L.AST.Operand
exprBuilder (CallExpr call         `As` _) = callBuilder call
exprBuilder (BranchExpr branch     `As` _) = branchBuilder branch
exprBuilder (CompoundExpr compound `As` _) = compoundBuilder compound

compoundBuilder :: Typed Compound -> Builder L.AST.Operand
compoundBuilder (OpCompound op           `As` _) = opBuilder op
compoundBuilder (PrimaryCompound primary `As` _) = primaryBuilder primary

primaryBuilder :: Typed Primary -> Builder L.AST.Operand
primaryBuilder (BlockPrimary block  `As` _) = blockBuilder block
primaryBuilder (TuplePrimary [expr] `As` _) = exprBuilder expr
primaryBuilder (IdentPrimary name   `As` _) = get >>= f
  where f context = case contextLookup name context of
            Just value -> pure value
            Nothing    -> error ("undefined name: " ++ T.unpack name)
primaryBuilder (IntegerPrimary x    `As` _) = IR.int32 x
primaryBuilder primary = error ("unhandled primary:\n\n" ++ ppShow primary)

defBuilder :: Typed Def -> Builder L.AST.Operand
defBuilder (Def name expr `As` _) = do
    e <- exprBuilder expr
    modify (contextInsert name e)
    pure e

callBuilder :: Typed Call -> Builder L.AST.Operand
callBuilder (Call callable (CompoundExpr (PrimaryCompound (TuplePrimary args `As` _) `As` _) `As` _) `As` _) = do
    fn <- compoundBuilder callable
    as <- traverse exprBuilder args
    IR.call fn (fmap (, []) as)
callBuilder (Call callable (CompoundExpr (PrimaryCompound primary@(IntegerPrimary _ `As` _) `As` _) `As` _) `As` _) = do
    fn <- compoundBuilder callable
    p <- primaryBuilder primary
    IR.call fn [(p, [])]
callBuilder call = error ("unhandled call:\n\n" ++ ppShow call)

branchBuilder :: Typed Branch -> Builder L.AST.Operand
branchBuilder (Branch cond trueBranch falseBranch `As` _) =
    join (IR.select <$> compoundBuilder cond
                    <*> exprBuilder trueBranch
                    <*> exprBuilder falseBranch)

opBuilder :: Typed Op -> Builder L.AST.Operand
opBuilder (Op op lhs rhs `As` _) = do
    l <- primaryBuilder lhs
    r <- compoundBuilder rhs
    case op of
        "+"  -> IR.add l r
        "-"  -> IR.sub l r
        "*"  -> IR.mul l r
        "==" -> IR.icmp L.AST.EQ l r
        "<"  -> IR.icmp L.AST.SLT l r
        o    -> error ("unhandled operator: " ++ show o)

blockBuilder :: Typed Block -> Builder L.AST.Operand
blockBuilder block = do
    nextID <- fromInteger <$> contextStateNextID
    lift $ functionBuilder (L.AST.UnName nextID) block

functionBuilder :: L.AST.Name -> Typed Block -> ModuleBuilder L.AST.Operand
functionBuilder lName (Block (ParamList params) statements `As` FunctionType _ retTy) =
    IR.function lName lArgs (fromType retTy) $ \argOperands -> do
        results <- locally $ do
            modify $ contextInsertScope (HM.fromList (zip argNames argOperands))
            traverse statementBuilder statements
        when (not (null results)) (IR.ret (last results))
  where f (argName `As` argTy) = (fromType argTy, IR.ParameterName . ShortBS.toShort . encodeUtf8 $ argName)
        lArgs    = f        <$> params
        argNames = typedVal <$> params
functionBuilder _ block  = error ("unhandled block:\n\n" ++ ppShow block)

fromType :: Type -> L.AST.Type
fromType (TupleType [])                  = L.AST.void
fromType (IntType 32)                    = L.AST.i32
fromType UnresolvedType                  = error "unresolved type"
fromType (UnresolvedPrimaryType primary) = error ("unresolved primary type:\n\n" ++ ppShow primary)
fromType ty                              = error ("unhandled type: " ++ show ty)
