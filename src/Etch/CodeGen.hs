{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Etch.CodeGen where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as ShortBS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T (pack)
import Data.Foldable (traverse_)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.State
import qualified LLVM              as L
import qualified LLVM.Context      as L
import qualified LLVM.AST          as L.AST hiding (type')
import qualified LLVM.AST.Constant as L.AST.Const
import qualified LLVM.AST.Global   as L.AST
import qualified LLVM.AST.Type     as L.AST (i32)
import qualified LLVM.IRBuilder.Constant    as IR
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module      as IR
import qualified LLVM.IRBuilder.Monad       as IR
import Etch.AST
import Etch.Module

codeGen m = L.withContext $ \ctx ->
    L.withModuleFromAST ctx lAST (fmap BS.unpack . L.moduleLLVMAssembly)
  where srcFile = (ShortBS.toShort . encodeUtf8 . T.pack . moduleSourceFile) m
        lAST = (moduleGen m) { L.AST.moduleSourceFileName = srcFile }

moduleGen :: Module -> L.AST.Module
moduleGen m = IR.buildModule name $ do
    traverse_ topLevelBuilder (moduleASTs m)
  where name = (ShortBS.toShort . encodeUtf8 . T.pack . moduleName) m

topLevelBuilder :: IR.MonadModuleBuilder m => AST -> m L.AST.Operand
topLevelBuilder (Call "=" (Tuple [Identifier name, Block asts])) = do
    IR.function nm [] L.AST.i32 $ \_ -> do
        results <- traverse (astBuilder HM.empty) asts
        when (not (null results)) (IR.ret (last results))
  where nm = L.AST.Name (ShortBS.toShort name)
topLevelBuilder (Call "=" (Tuple [Identifier name, IntegerLiteral x])) =
    IR.global nm L.AST.i32 constant
  where nm = L.AST.Name (ShortBS.toShort name)
        constant = L.AST.Const.Int 32 x
topLevelBuilder (Call "=" (Tuple [Identifier name, Call "->" (Tuple [Tuple args, Block asts])])) = do
    IR.function lName lArgs L.AST.i32 $ \argOperands -> do
        let params = HM.fromList (zip argNames argOperands)
        results <- traverse (astBuilder params) asts
        when (not (null results)) (IR.ret (last results))
  where lName = L.AST.Name (ShortBS.toShort name)
        argNames = [ x | Identifier x <- args ]
        lArgs = (L.AST.i32,) . IR.ParameterName . ShortBS.toShort <$> argNames
topLevelBuilder ast = error (show ast)

astBuilder :: IR.MonadIRBuilder m => HM.HashMap BS.ByteString L.AST.Operand -> AST -> m L.AST.Operand
astBuilder params (Call "+" (Tuple [lhs, rhs])) = do
    l <- astBuilder params lhs
    r <- astBuilder params rhs
    IR.add l r
astBuilder params (Call "-" (Tuple [lhs, rhs])) = do
    l <- astBuilder params lhs
    r <- astBuilder params rhs
    IR.sub l r
astBuilder params (Call "*" (Tuple [lhs, rhs])) = do
    l <- astBuilder params lhs
    r <- astBuilder params rhs
    IR.mul l r
astBuilder params (Tuple [ast]) = astBuilder params ast
astBuilder params (Identifier name) = case HM.lookup name params of
    Just operand -> pure operand
    Nothing      -> error ("undefined name: " ++ BS.unpack name)
astBuilder _ (IntegerLiteral x) = IR.int32 x
astBuilder _ ast = error (show ast)
