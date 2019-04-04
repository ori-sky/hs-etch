{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Etch.Analysis.Resolution where

import qualified Data.HashMap.Lazy as HM
import Data.Text (unpack)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens (view)
import Etch.Types.Analysis
import Etch.Types.ErrorContext
import Etch.Types.Lenses
import Etch.Types.SemanticTree

type MonadAnalysis m = (MonadError ErrorContext m, MonadReader AnalysisState m)

analysis :: MonadAnalysis m => [Typed Statement] -> m [Typed Statement]
analysis statements = traverse statementAnalysis statements

statementAnalysis :: MonadAnalysis m => Typed Statement -> m (Typed Statement)
statementAnalysis (DefStatement def   `As` _) = tymap DefStatement  <$> defAnalysis def
statementAnalysis (ExprStatement expr `As` _) = tymap ExprStatement <$> exprAnalysis expr

exprAnalysis :: MonadAnalysis m => Typed Expr -> m (Typed Expr)
--exprAnalysis (CallExpr call         `As` _) = tymap CallExpr     <$> callAnalysis call
--exprAnalysis (BranchExpr branch     `As` _) = tymap BranchExpr   <$> branchAnalysis branch
exprAnalysis (CompoundExpr compound `As` _) = tymap CompoundExpr <$> compoundAnalysis compound
exprAnalysis t = pure t

compoundAnalysis :: MonadAnalysis m => Typed Compound -> m (Typed Compound)
compoundAnalysis (OpCompound op           `As` _) = tymap OpCompound      <$> opAnalysis op
compoundAnalysis (PrimaryCompound primary `As` _) = tymap PrimaryCompound <$> primaryAnalysis primary

primaryAnalysis :: MonadAnalysis m => Typed Primary -> m (Typed Primary)
primaryAnalysis (BlockPrimary block     `As` _) = tymap BlockPrimary <$> blockAnalysis block
primaryAnalysis (NewPrimary newID exprs `As` NewType typeID _) = do
    typeds <- traverse exprAnalysis exprs
    pure $ NewPrimary newID typeds `As` NewType typeID (typedTy <$> typeds)
primaryAnalysis (NewPrimary newID exprs `As` UnresolvedPrimaryType primary) = do
    typeds <- traverse exprAnalysis exprs
    p <- primaryAnalysis primary
    pure $ NewPrimary newID typeds `As` (typedTy p)
--primaryAnalysis (TypePrimary ty     `As` _) = tymap TypePrimary  <$> typeAnalysis ty
--primaryAnalysis (TuplePrimary exprs `As` _) = do
--    typeds <- traverse exprAnalysis exprs
--    pure $ TuplePrimary typeds `As` TupleType (typedTy <$> typeds)
--primaryAnalysis (NewPrimary _       `As` _)              = error "new analysis not implemented yet"
--    typeds <- traverse exprAnalysis exprs
--    newID <- use nextID
--    nextID %= succ
--    pure $ NewPrimary typeds `As` NewType newID (typedTy <$> typeds)
--primaryAnalysis (IdentPrimary ident `As` UnresolvedType) = error ("type resolution not implemented yet (" ++ unpack ident ++ ")")
primaryAnalysis (IdentPrimary "int" `As` UnresolvedType) = pure (IdentPrimary "int" `As` IntType 32)
primaryAnalysis (IdentPrimary name  `As` UnresolvedType) = do
    hm <- view scope
    let resolvedTy = case HM.lookup name hm of
            Nothing          -> UnresolvedType
            Just (Term ty _) -> ty
    pure (IdentPrimary name `As` resolvedTy)
primaryAnalysis t = pure t

defAnalysis :: MonadAnalysis m => Typed Def -> m (Typed Def)
defAnalysis (Def name expr `As` _) = tymap (Def name) <$> exprAnalysis expr

blockAnalysis :: MonadAnalysis m => Typed Block -> m (Typed Block)
blockAnalysis (Block (ParamList params) statements `As` _) = do
    args <- traverse paramAnalysis params
    s <- traverse statementAnalysis statements
    let retTy = if null s then TupleType [] else typedTy (last s)
        paramTys = typedTy <$> args
    pure $ Block (ParamList args) s `As` FunctionType paramTys retTy

opAnalysis :: MonadAnalysis m => Typed Op -> m (Typed Op)
opAnalysis = undefined

branchAnalysis :: MonadAnalysis m => Typed Branch -> m (Typed Branch)
branchAnalysis = undefined

callAnalysis :: MonadAnalysis m => Typed Call -> m (Typed Call)
callAnalysis = undefined

typeAnalysis :: MonadAnalysis m => Typed Type -> m (Typed Type)
typeAnalysis = undefined

paramAnalysis :: MonadAnalysis m => Typed Param -> m (Typed Param)
paramAnalysis (name `As` UnresolvedType) = error ("parameter type inference not implemented yet (" ++ unpack name ++ ")")
paramAnalysis (name `As` UnresolvedPrimaryType primary) = (name `As`) . typedTy <$> primaryAnalysis primary
paramAnalysis t = pure t
