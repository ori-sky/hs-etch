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
import qualified LLVM.AST.Type              as L.AST (Type(IntegerType), void, ptr, i32)
import qualified LLVM.AST.Typed             as L.AST (typeOf)
--import qualified LLVM.IRBuilder.Constant    as IR
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
        moduleBuilder = do
            traverse_ topLevelDefBuilder (moduleDefs m)

topLevelDefBuilder :: Typed Def -> ModuleBuilder (Maybe L.AST.Operand)
topLevelDefBuilder (Def name (FunctionExpr function `As` _) `As` _) = do
    fn <- topLevelFunctionBuilder lName function
    modify (contextInsert name fn)
    pure (Just fn)
  where lName = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
topLevelDefBuilder (Def name (CompoundExpr (PrimaryCompound (IntegerPrimary x `As` _) `As` _) `As` _) `As` IntType 32) = do
    modify (contextInsert name constantOp)
    Just <$> IR.global lName L.AST.i32 constant
  where lName      = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
        constant   = L.AST.Const.Int 32 x
        constantOp = L.AST.ConstantOperand constant
topLevelDefBuilder (Def name (CompoundExpr (PrimaryCompound (IntegerPrimary 0 `As` _) `As` _) `As` _) `As` PtrType ty) = do
    modify (contextInsert name constantOp)
    Just <$> IR.global lName lType constant
  where lName      = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
        lType      = L.AST.ptr (fromType ty)
        constant   = L.AST.Const.Null lType
        constantOp = L.AST.ConstantOperand constant
topLevelDefBuilder (Def name (CompoundExpr (PrimaryCompound (IntegerPrimary x `As` _) `As` _) `As` _) `As` PtrType ty) = do
    modify (contextInsert name constantOp)
    Just <$> IR.global lName lType constant
  where lName      = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
        lType      = L.AST.ptr (fromType ty)
        constant   = L.AST.Const.IntToPtr (L.AST.Const.Int 32 x) lType
        constantOp = L.AST.ConstantOperand constant
topLevelDefBuilder (_ `As` NewType _ _)   = pure Nothing
topLevelDefBuilder (_ `As` BuiltinType _) = pure Nothing
topLevelDefBuilder (_ `As` PrimaryType primary) = error ("unresolved primary type:\n\n" ++ ppShow primary)
topLevelDefBuilder def = error ("unhandled top-level def:\n\n" ++ ppShow def)

topLevelFunctionBuilder :: L.AST.Name -> Typed Function -> ModuleBuilder L.AST.Operand
topLevelFunctionBuilder lName (Function (ParamList params) expr `As` FunctionType _ retTy) =
    IR.function lName lArgs (fromType retTy) $ \argOperands -> do
        result <- locally $ do
            modify $ contextInsertScope (HM.fromList (zip argNames argOperands))
            exprBuilder expr
        case L.AST.typeOf result of
            L.AST.VoidType -> IR.retVoid
            _              -> IR.ret result
  where f (argName `As` argTy) = (fromType argTy, IR.ParameterName . ShortBS.toShort . encodeUtf8 $ argName)
        lArgs    = f        <$> params
        argNames = typedVal <$> params
topLevelFunctionBuilder _ function  = error ("unhandled function:\n\n" ++ ppShow function)

statementBuilder :: Typed Statement -> Builder L.AST.Operand
statementBuilder (DefStatement def   `As` _) = defBuilder def
statementBuilder (ForeignStatement f `As` _) = foreignBuilder f
statementBuilder (ExprStatement expr `As` _) = exprBuilder expr

exprBuilder :: Typed Expr -> Builder L.AST.Operand
exprBuilder (FunctionExpr function `As` _) = do
    nextID <- fromInteger <$> contextStateNextID
    lift $ topLevelFunctionBuilder (L.AST.UnName nextID) function
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
primaryBuilder (IntegerPrimary x    `As` IntType n)  = pure constantOp
  where constant   = L.AST.Const.Int (fromInteger n) x
        constantOp = L.AST.ConstantOperand constant
primaryBuilder (IntegerPrimary x    `As` PtrType ty) = pure constantOp
  where constant   = L.AST.Const.IntToPtr (L.AST.Const.Int 32 x) (fromType ty)
        constantOp = L.AST.ConstantOperand constant
primaryBuilder primary = error ("unhandled primary:\n\n" ++ ppShow primary)

defBuilder :: Typed Def -> Builder L.AST.Operand
defBuilder (Def name expr `As` _) = do
    op <- exprBuilder expr
    modify (contextInsert name op)
    pure op

foreignBuilder :: Typed Foreign -> Builder L.AST.Operand
foreignBuilder (Foreign name `As` FunctionType tys retTy) = do
    op <- lift $ IR.extern lName (fromType <$> tys) (fromType retTy)
    modify (contextInsert name op)
    pure op
  where lName = (L.AST.Name . ShortBS.toShort . encodeUtf8) name
foreignBuilder f = error ("unhandled foreign:\n\n" ++ ppShow f)

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
        ">"  -> IR.icmp L.AST.SGT l r
        "|+" -> IR.gep l [r]
        "|*" -> do
            g <- IR.gep l [r]
            IR.load g 0
        "|=" -> do
            IR.store l 0 r
            pure l
        o    -> error ("unhandled operator: " ++ show o)

blockBuilder :: Typed Block -> Builder L.AST.Operand
blockBuilder (Block [] `As` _) = pure $ L.AST.ConstantOperand (L.AST.Const.Undef L.AST.void)
blockBuilder (Block statements `As` _) = do
    --nextID <- fromInteger <$> contextStateNextID
    last <$> traverse statementBuilder statements
    --lift $ topLevelFunctionBuilder (L.AST.UnName nextID) block

fromType :: Type -> L.AST.Type
fromType (TupleType [])                          = L.AST.void
fromType (TupleType [ty])                        = fromType ty
fromType (PrimaryType (TuplePrimary [] `As` ty)) = fromType ty
fromType (PtrType ty)                            = L.AST.ptr (fromType ty)
fromType (IntType n)                             = L.AST.IntegerType (fromInteger n)
fromType (FunctionType tys retTy)                = L.AST.FunctionType (fromType retTy) (fromType <$> tys) False
fromType UnresolvedType                          = error "unresolved type"
fromType (PrimaryType primary)                   = error ("unresolved primary type:\n\n" ++ ppShow primary)
fromType ty                                      = error ("unhandled type: " ++ show ty)
