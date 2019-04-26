{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Etch.Analysis.Resolution where

import qualified Data.HashMap.Lazy as HM
import Control.Monad.Except
import Control.Monad.State
import Control.Lens (use, (%=))
import Text.Show.Pretty (ppShow)
import Etch.Types.Analysis
import Etch.Types.ErrorContext
import Etch.Types.Lenses
import Etch.Types.SemanticTree

type MonadAnalysis m = (MonadError ErrorContext m, MonadState AnalysisState m)

analysis :: MonadAnalysis m => [Typed Statement] -> m [Typed Statement]
analysis statements = traverse statementAnalysis statements

statementAnalysis :: MonadAnalysis m => Typed Statement -> m (Typed Statement)
statementAnalysis (DefStatement def   `As` _) = tymap DefStatement     <$> defAnalysis def
statementAnalysis (ForeignStatement f `As` _) = tymap ForeignStatement <$> foreignAnalysis f
statementAnalysis (ExprStatement expr `As` _) = tymap ExprStatement    <$> exprAnalysis expr

exprAnalysis :: MonadAnalysis m => Typed Expr -> m (Typed Expr)
exprAnalysis (FunctionExpr function `As` _) = tymap FunctionExpr <$> functionAnalysis function
exprAnalysis (CallExpr (Call (_ `As` BuiltinType IntNBuiltin) (CompoundExpr (PrimaryCompound (IntegerPrimary n `As` _) `As` _) `As` _) `As` _) `As` _) =
    pure $ tymap CompoundExpr $ tymap PrimaryCompound (BuiltinPrimary builtin `As` BuiltinType builtin)
  where builtin = IntTypeBuiltin n
exprAnalysis (CallExpr (Call (_ `As` BuiltinType PtrBuiltin) (CompoundExpr (PrimaryCompound primary `As` _) `As` _) `As` _) `As` _) = do
    p <- primaryAnalysis primary
    let builtin = PtrTypeBuiltin (PrimaryType p)
    pure $ tymap CompoundExpr $ tymap PrimaryCompound (BuiltinPrimary builtin `As` BuiltinType builtin)
exprAnalysis (CallExpr call         `As` _) = tymap CallExpr     <$> callAnalysis call
exprAnalysis (BranchExpr branch     `As` _) = tymap BranchExpr   <$> branchAnalysis branch
exprAnalysis (CompoundExpr compound `As` _) = tymap CompoundExpr <$> compoundAnalysis compound

compoundAnalysis :: MonadAnalysis m => Typed Compound -> m (Typed Compound)
compoundAnalysis (OpCompound op           `As` _) = tymap OpCompound      <$> opAnalysis op
compoundAnalysis (PrimaryCompound primary `As` _) = tymap PrimaryCompound <$> primaryAnalysis primary

primaryAnalysis :: MonadAnalysis m => Typed Primary -> m (Typed Primary)
primaryAnalysis (BlockPrimary block     `As` _) = tymap BlockPrimary <$> blockAnalysis block
primaryAnalysis (NewPrimary newID exprs `As` NewType typeID _) = do
    typeds <- traverse exprAnalysis exprs
    pure $ NewPrimary newID typeds `As` NewType typeID (typedTy <$> typeds)
primaryAnalysis (NewPrimary newID exprs `As` PrimaryType primary) = do
    typeds <- traverse exprAnalysis exprs
    p <- primaryAnalysis primary
    pure $ NewPrimary newID typeds `As` (typedTy p)
--primaryAnalysis (TypePrimary ty     `As` _) = tymap TypePrimary  <$> typeAnalysis ty
primaryAnalysis (TuplePrimary exprs `As` _) = do
    typeds <- traverse exprAnalysis exprs
    pure $ TuplePrimary typeds `As` TupleType (typedTy <$> typeds)
--primaryAnalysis (NewPrimary _       `As` _)              = error "new analysis not implemented yet"
--    typeds <- traverse exprAnalysis exprs
--    newID <- use nextID
--    nextID %= succ
--    pure $ NewPrimary typeds `As` NewType newID (typedTy <$> typeds)
--primaryAnalysis (IdentPrimary ident `As` UnresolvedType) = error ("type resolution not implemented yet (" ++ unpack ident ++ ")")
primaryAnalysis (IdentPrimary "intn" `As` UnresolvedType) = pure (BuiltinPrimary builtin `As` BuiltinType builtin)
  where builtin = IntNBuiltin
primaryAnalysis (IdentPrimary "ptr"  `As` UnresolvedType) = pure (BuiltinPrimary builtin `As` BuiltinType builtin)
  where builtin = PtrBuiltin
primaryAnalysis (IdentPrimary name  `As` _) = do
    hm <- use scope
    let resolvedTy = case HM.lookup name hm of
            Nothing          -> UnresolvedType
            Just (Term ty _) -> ty
    pure (IdentPrimary name `As` resolvedTy)
primaryAnalysis (IntegerPrimary x `As` ty) = (IntegerPrimary x `As`) <$> typeAnalysis ty
primaryAnalysis t = pure t

defAnalysis :: MonadAnalysis m => Typed Def -> m (Typed Def)
defAnalysis (Def name expr `As` _) = do
    e <- exprAnalysis expr
    scope %= HM.insert name (Term (typedTy e) HM.empty)
    pure $ tymap (Def name) e

foreignAnalysis :: MonadAnalysis m => Typed Foreign -> m (Typed Foreign)
foreignAnalysis typed = pure typed

functionAnalysis :: MonadAnalysis m => Typed Function -> m (Typed Function)
functionAnalysis (Function (ParamList params) expr `As` _) = do
    args <- traverse paramAnalysis params
    e <- exprAnalysis expr
    paramTys <- traverse typeAnalysis (typedTy <$> args)
    pure $ Function (ParamList args) e `As` FunctionType paramTys (typedTy e)

blockAnalysis :: MonadAnalysis m => Typed Block -> m (Typed Block)
--blockAnalysis (Block (ParamList params) statements `As` _) = do
--    args <- traverse paramAnalysis params
--    s <- traverse statementAnalysis statements
--    let retTy = if null s then TupleType [] else typedTy (last s)
--        paramTys = typedTy <$> args
--    pure $ Block (ParamList args) s `As` FunctionType paramTys retTy
blockAnalysis (Block statements `As` _) = do
    s <- traverse statementAnalysis statements
    let retTy = if null s then TupleType [] else typedTy (last s)
    pure $ Block s `As` retTy

opAnalysis :: MonadAnalysis m => Typed Op -> m (Typed Op)
opAnalysis (Op op lhs rhs `As` _) = do
    l <- primaryAnalysis lhs
    r <- compoundAnalysis rhs
    pure (Op op l r `As` typedTy l)

callAnalysis :: MonadAnalysis m => Typed Call -> m (Typed Call)
callAnalysis (Call callable expr `As` _) = do
    c <- compoundAnalysis callable
    e <- exprAnalysis expr
    (Call c e `As`) <$> callTypeAnalysis (typedTy c)

callTypeAnalysis :: MonadAnalysis m => Type -> m Type
callTypeAnalysis (TupleType [ty])          = callTypeAnalysis ty
callTypeAnalysis (FunctionType _ retTy)    = typeAnalysis retTy
callTypeAnalysis (BuiltinType IntNBuiltin) = pure (BuiltinType IntNBuiltin)
callTypeAnalysis (BuiltinType PtrBuiltin)  = pure (BuiltinType PtrBuiltin)
callTypeAnalysis UnresolvedType            = pure (UnresolvedType)
callTypeAnalysis ty                        = throwError $ ErrorContext "type is not callable" [ppShow ty]

branchAnalysis :: MonadAnalysis m => Typed Branch -> m (Typed Branch)
branchAnalysis (Branch cond trueBranch falseBranch `As` _) = do
    c <- compoundAnalysis cond
    t <- exprAnalysis trueBranch
    f <- exprAnalysis falseBranch
    pure (Branch c t f `As` typedTy t)

typeAnalysis :: MonadAnalysis m => Type -> m Type
typeAnalysis (PrimaryType (_ `As` BuiltinType (IntTypeBuiltin n))) = pure (IntType n)
typeAnalysis (PrimaryType (_ `As` BuiltinType (PtrTypeBuiltin ty))) = PtrType <$> typeAnalysis ty
typeAnalysis (PrimaryType primary) = PrimaryType <$> primaryAnalysis primary
typeAnalysis ty = pure ty

paramAnalysis :: MonadAnalysis m => Typed Param -> m (Typed Param)
paramAnalysis (name `As` UnresolvedType) = do
    scope %= HM.insert name (Term intType HM.empty) -- XXX: need scopes
    pure (name `As` intType)
  where intType = IntType 32
--paramAnalysis (name `As` UnresolvedType) = error ("parameter type inference not implemented yet (" ++ unpack name ++ ")")
paramAnalysis (name `As` ty@(PrimaryType _)) = do
    t <- typeAnalysis ty
    scope %= HM.insert name (Term t HM.empty) -- XXX: need scopes
    pure (name `As` t)
paramAnalysis t = pure t
