{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Etch.Analysis.Semantics where

import qualified Data.HashMap.Lazy as HM
import Control.Monad.Except
import Control.Monad.State
import Control.Lens (use, (%=))
import Text.Show.Pretty (ppShow)
import qualified Etch.Types.SyntaxTree as Syntax
import Etch.Types.Analysis
import Etch.Types.ErrorContext
import Etch.Types.Lenses
import Etch.Types.SemanticTree

type MonadAnalysis m = (MonadError ErrorContext m, MonadState AnalysisState m)

analysis :: MonadAnalysis m => [Syntax.Statement] -> m [Typed SemBox]
analysis statements = traverse statementAnalysis statements

statementAnalysis :: MonadAnalysis m => Syntax.Statement -> m (Typed SemBox)
statementAnalysis (Syntax.DefStatement def)   = defAnalysis def
statementAnalysis (Syntax.ForeignStatement f) = foreignAnalysis f
statementAnalysis (Syntax.ExprStatement expr) = exprAnalysis expr

exprAnalysis :: MonadAnalysis m => Syntax.Expr -> m (Typed SemBox)
exprAnalysis (Syntax.FunctionExpr function) = functionAnalysis function
exprAnalysis (Syntax.CallExpr call)         = callAnalysis call
exprAnalysis (Syntax.BranchExpr branch)     = branchAnalysis branch
exprAnalysis (Syntax.CompoundExpr compound) = compoundAnalysis compound

compoundAnalysis :: MonadAnalysis m => Syntax.Compound -> m (Typed SemBox)
compoundAnalysis (Syntax.OpCompound op)     = opAnalysis op
compoundAnalysis (Syntax.AtomCompound atom) = atomAnalysis atom

atomAnalysis :: MonadAnalysis m => Syntax.Atom -> m (Typed SemBox)
atomAnalysis (Syntax.SigAtom (Syntax.Sig primary atom)) = do
    pVal `As` _ <- primaryAnalysis primary
    atomAnalysis atom >>= \case
        t@(_ `As` UnresolvedType) -> pure $ pVal `As` SemType t
        typed                     -> throwError $ ErrorContext "not a type" [ppShow typed]
atomAnalysis (Syntax.PrimaryAtom primary) = primaryAnalysis primary

primaryAnalysis :: MonadAnalysis m => Syntax.Primary -> m (Typed SemBox)
primaryAnalysis (Syntax.BlockPrimary block) = blockAnalysis block
primaryAnalysis (Syntax.TuplePrimary [expr]) = exprAnalysis expr
primaryAnalysis (Syntax.TuplePrimary exprs) = do
    typeds <- traverse exprAnalysis exprs
    pure $ SemBox (Tuple typeds) `As` TupleType (typedTy <$> typeds)
primaryAnalysis (Syntax.NewPrimary exprs) = do
    typeds <- traverse exprAnalysis exprs
    newID <- use nextID
    nextID %= succ
    pure $ SemBox (New newID typeds) `As` NewType newID (typedTy <$> typeds)
primaryAnalysis (Syntax.IdentPrimary ident) = pure $ SemBox (IdentPrimary ident) `As` UnresolvedType
primaryAnalysis (Syntax.IntegerPrimary x)   = pure $ SemBox (IntegerPrimary x)   `As` IntType 32
primaryAnalysis (Syntax.StringPrimary s)    = pure $ SemBox (StringPrimary s)    `As` StringType

defAnalysis :: MonadAnalysis m => Syntax.Def -> m (Typed SemBox)
defAnalysis (Syntax.Def name expr) = do
    e <- exprAnalysis expr
    scope %= HM.insert name (Term (typedTy e) HM.empty)
    pure $ tymap (SemBox . Def name) e

foreignAnalysis :: MonadAnalysis m => Syntax.Foreign -> m (Typed SemBox)
foreignAnalysis (Syntax.Foreign (Syntax.Sig name atom)) = do
    a <- atomAnalysis atom
    scope %= HM.insert name (Term (SemType a) HM.empty) -- XXX: need scopes
    pure $ SemBox (Foreign name) `As` SemType a

functionAnalysis :: MonadAnalysis m => Syntax.Function -> m (Typed SemBox)
functionAnalysis (Syntax.Function (Syntax.ParamList params) expr) = do
    args <- traverse paramAnalysis params
    e <- exprAnalysis expr
    let paramTys = typedTy <$> args
    pure $ SemBox (Function (ParamList args) e) `As` FunctionType paramTys (typedTy e)

callAnalysis :: MonadAnalysis m => Syntax.Call -> m (Typed SemBox)
callAnalysis (Syntax.Call callable expr) = do
    c <- compoundAnalysis callable
    e <- exprAnalysis expr
    ct <- callTypeAnalysis (typedTy c)
    pure $ SemBox (Call c e) `As` ct

callTypeAnalysis :: MonadAnalysis m => Type -> m Type
callTypeAnalysis (FunctionType _ retTy)              = pure retTy
callTypeAnalysis (BuiltinType FunctionBuiltin)       = pure (BuiltinType FunctionBuiltin)
callTypeAnalysis (BuiltinType (Function2Builtin ty)) = pure (BuiltinType (Function2Builtin ty))
callTypeAnalysis (BuiltinType IntNBuiltin)           = pure (BuiltinType IntNBuiltin)
callTypeAnalysis (BuiltinType PtrBuiltin)            = pure (BuiltinType PtrBuiltin)
callTypeAnalysis UnresolvedType                      = pure (UnresolvedType)
callTypeAnalysis ty                                  = throwError $ ErrorContext "type is not callable" [ppShow ty]

branchAnalysis :: MonadAnalysis m => Syntax.Branch -> m (Typed SemBox)
branchAnalysis (Syntax.Branch cond trueBranch falseBranch) = do
    c <- compoundAnalysis cond
    t <- exprAnalysis trueBranch
    f <- exprAnalysis falseBranch
    pure $ SemBox (Branch c t f) `As` typedTy t

opAnalysis :: MonadAnalysis m => Syntax.Op -> m (Typed SemBox)
opAnalysis (Syntax.Op op lhs rhs) = do
    l <- atomAnalysis lhs
    r <- compoundAnalysis rhs
    pure $ SemBox (Op op l r) `As` typedTy l

blockAnalysis :: MonadAnalysis m => Syntax.Block -> m (Typed SemBox)
blockAnalysis (Syntax.Block statements) = do
    s <- traverse statementAnalysis statements
    let retTy = if null s then TupleType [] else typedTy (last s)
    pure $ SemBox (Block s) `As` retTy

paramAnalysis :: MonadAnalysis m => Syntax.Param -> m (Typed Param)
paramAnalysis (Syntax.SigParam (Syntax.Sig name atom)) = do
    a <- atomAnalysis atom
    scope %= HM.insert name (Term (typedTy a) HM.empty) -- XXX: need scopes
    pure (name `As` SemType a)
-- paramAnalysis (Syntax.SigParam (Syntax.AtomSig name ty)) = atomAnalysis >>= \case
--     TypePrimary t `As` _ ->

--        TypePrimary (expectedTy `As` _) `As` _ -> if actualTy == expectedTy || actualTy == UnresolvedType
--            then pure (pVal `As` expectedTy)
--            else fail ("expected type `" ++ show expectedTy ++ "` does not match actual type `" ++ show actualTy ++ "`: " ++ show sig)
paramAnalysis (Syntax.InferredParam name)            = pure (name `As` UnresolvedType)
