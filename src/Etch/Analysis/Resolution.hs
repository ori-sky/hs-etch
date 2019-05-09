{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Etch.Analysis.Resolution where

import qualified Data.HashMap.Lazy as HM
--import Control.Monad.Except
--import Control.Monad.State
import Control.Lens (use, (%=))
--import Text.Show.Pretty (ppShow)
--import Etch.Types.ErrorContext
import Etch.Types.Lenses
import Etch.Types.Semantic hiding (visitParam, visitType)

analysis :: MonadAnalysis m => Analysis m
analysis = Analysis { blockAnalysis    = visitBlock
                    , branchAnalysis   = undefined --visitBranch
                    , callAnalysis     = visitCall
                    , defAnalysis      = visitDef
                    , foreignAnalysis  = visitForeign
                    , functionAnalysis = visitFunction
                    , newAnalysis      = undefined --visitNew
                    , opAnalysis       = visitOp
                    , paramAnalysis    = visitParam
                    , primaryAnalysis  = visitPrimary
                    , tupleAnalysis    = visitTuple
                    , typeAnalysis     = visitType
                    }

visitBlock :: MonadAnalysis m => Typed Block -> m (Typed SemBox)
visitBlock (block `As` ty) = pure (SemBox block `As` ty)

visitCall :: MonadAnalysis m => Typed Call -> m (Typed SemBox)
visitCall (call `As` ty) = pure (SemBox call `As` ty)

visitDef :: MonadAnalysis m => Typed Def -> m (Typed SemBox)
visitDef (Def name expr `As` ty) = do
    scope %= HM.insert name (Term ty HM.empty)
    pure $ SemBox (Def name expr) `As` ty

visitForeign :: MonadAnalysis m => Typed Foreign -> m (Typed SemBox)
visitForeign (Foreign name `As` ty) = do
    scope %= HM.insert name (Term ty HM.empty) -- XXX: need scopes
    pure $ SemBox (Foreign name) `As` ty

visitFunction :: MonadAnalysis m => Typed Function -> m (Typed SemBox)
visitFunction (fn `As` ty) = pure (SemBox fn `As` ty)

visitOp :: MonadAnalysis m => Typed Op -> m (Typed SemBox)
visitOp (op `As` ty) = pure (SemBox op `As` ty)

visitParam :: MonadAnalysis m => Typed Param -> m (Typed Param)
visitParam (name `As` UnresolvedType) = do
    scope %= HM.insert name (Term intType HM.empty) -- XXX: need scopes
    pure (name `As` intType)
  where intType = IntType 32
visitParam (name `As` ty) = do
    scope %= HM.insert name (Term ty HM.empty) -- XXX: need scopes
    pure (name `As` ty)

visitPrimary :: MonadAnalysis m => Typed Primary -> m (Typed SemBox)
--visitPrimary (NewPrimary newID exprs `As` NewType typeID _) = do
--    typeds <- traverse exprAnalysis exprs
--    pure $ NewPrimary newID typeds `As` NewType typeID (typedTy <$> typeds)
--visitPrimary (NewPrimary newID exprs `As` PrimaryType primary) = do
--    typeds <- traverse exprAnalysis exprs
--    p <- visitPrimary primary
--    pure $ NewPrimary newID typeds `As` (typedTy p)
visitPrimary (IdentPrimary "function" `As` UnresolvedType) = pure $ SemBox (BuiltinPrimary builtin) `As` BuiltinType builtin
  where builtin = FunctionBuiltin
visitPrimary (IdentPrimary "intn"     `As` UnresolvedType) = pure $ SemBox (BuiltinPrimary builtin) `As` BuiltinType builtin
  where builtin = IntNBuiltin
visitPrimary (IdentPrimary "ptr"      `As` UnresolvedType) = pure $ SemBox (BuiltinPrimary builtin) `As` BuiltinType builtin
  where builtin = PtrBuiltin
visitPrimary (IdentPrimary name  `As` _) = do
    hm <- use scope
    let resolvedTy = case HM.lookup name hm of
            Nothing          -> UnresolvedType
            Just (Term ty _) -> ty
    pure $ SemBox (IdentPrimary name) `As` resolvedTy
visitPrimary (p `As` t) = pure (SemBox p `As` t)

visitTuple :: MonadAnalysis m => Typed Tuple -> m (Typed SemBox)
visitTuple (tuple `As` ty) = pure (SemBox tuple `As` ty)

visitType :: MonadAnalysis m => Type -> m Type
visitType (SemType (_         `As` BuiltinType (FunctionTypeBuiltin ty retTy))) = pure (FunctionType [ty] retTy)
visitType (SemType (_         `As` TupleType []))                               = pure (TupleType [])
visitType (SemType (_         `As` BuiltinType (IntTypeBuiltin n)))             = pure (IntType n)
visitType (SemType (_         `As` BuiltinType (PtrTypeBuiltin ty)))            = PtrType     <$> visitType ty
--visitType (SemType primary)                                                     = PrimaryType <$> primaryAnalysis primary
visitType ty = pure ty

{-

exprAnalysis :: MonadAnalysis m => Typed Expr -> m (Typed Expr)
exprAnalysis (FunctionExpr function `As` _) = tymap FunctionExpr <$> functionAnalysis function
exprAnalysis (CallExpr (Call (_ `As` BuiltinType FunctionBuiltin) (CompoundExpr (PrimaryCompound primary `As` _) `As` _) `As` _) `As` _) = do
    builtin <- Function2Builtin <$> typeAnalysis (PrimaryType primary)
    pure $ tymap CompoundExpr $ tymap PrimaryCompound (BuiltinPrimary builtin `As` BuiltinType builtin)
exprAnalysis (CallExpr (Call (_ `As` BuiltinType (Function2Builtin ty)) (CompoundExpr (PrimaryCompound primary `As` _) `As` _) `As` _) `As` _) = do
    builtin <- FunctionTypeBuiltin <$> typeAnalysis ty <*> typeAnalysis (PrimaryType primary)
    pure $ tymap CompoundExpr $ tymap PrimaryCompound (BuiltinPrimary builtin `As` BuiltinType builtin)
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

branchAnalysis :: MonadAnalysis m => Typed Branch -> m (Typed Branch)
branchAnalysis (Branch cond trueBranch falseBranch `As` _) = do
    c <- compoundAnalysis cond
    t <- exprAnalysis trueBranch
    f <- exprAnalysis falseBranch
    pure (Branch c t f `As` typedTy t)
-}
