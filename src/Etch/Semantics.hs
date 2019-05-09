{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Etch.Semantics where

import qualified Data.HashMap.Lazy as HM
import Control.Monad.Except
import Control.Lens (use, (%=))
import Text.Show.Pretty (ppShow)
import qualified Etch.Types.SyntaxTree as Syntax
import Etch.Types.ErrorContext
import Etch.Types.Lenses
import Etch.Types.Semantic hiding (visitCallType, visitParam)

analysis :: MonadSemantic m => [Syntax.Statement] -> m [Typed SemBox]
analysis statements = traverse visitStatement statements

visitStatement :: MonadSemantic m => Syntax.Statement -> m (Typed SemBox)
visitStatement (Syntax.DefStatement def)   = visitDef def
visitStatement (Syntax.ForeignStatement f) = visitForeign f
visitStatement (Syntax.ExprStatement expr) = visitExpr expr

visitExpr :: MonadSemantic m => Syntax.Expr -> m (Typed SemBox)
visitExpr (Syntax.FunctionExpr function) = visitFunction function
visitExpr (Syntax.CallExpr call)         = visitCall call
visitExpr (Syntax.BranchExpr branch)     = visitBranch branch
visitExpr (Syntax.CompoundExpr compound) = visitCompound compound

visitCompound :: MonadSemantic m => Syntax.Compound -> m (Typed SemBox)
visitCompound (Syntax.OpCompound op)     = visitOp op
visitCompound (Syntax.AtomCompound atom) = visitAtom atom

visitAtom :: MonadSemantic m => Syntax.Atom -> m (Typed SemBox)
visitAtom (Syntax.SigAtom (Syntax.Sig primary atom)) = do
    pVal `As` _ <- visitPrimary primary
    visitAtom atom >>= \case
        t@(_ `As` UnresolvedType) -> pure $ pVal `As` SemType t
        typed                     -> throwError $ ErrorContext "not a type" [ppShow typed]
visitAtom (Syntax.PrimaryAtom primary) = visitPrimary primary

visitPrimary :: MonadSemantic m => Syntax.Primary -> m (Typed SemBox)
visitPrimary (Syntax.BlockPrimary block) = visitBlock block
visitPrimary (Syntax.TuplePrimary [expr]) = visitExpr expr
visitPrimary (Syntax.TuplePrimary exprs) = do
    typeds <- traverse visitExpr exprs
    pure $ SemBox (Tuple typeds) `As` TupleType (typedTy <$> typeds)
visitPrimary (Syntax.NewPrimary exprs) = do
    typeds <- traverse visitExpr exprs
    newID <- use nextID
    nextID %= succ
    pure $ SemBox (New newID typeds) `As` NewType newID (typedTy <$> typeds)
visitPrimary (Syntax.IdentPrimary ident) = pure $ SemBox (IdentPrimary ident) `As` UnresolvedType
visitPrimary (Syntax.IntegerPrimary x)   = pure $ SemBox (IntegerPrimary x)   `As` IntType 32
visitPrimary (Syntax.StringPrimary s)    = pure $ SemBox (StringPrimary s)    `As` StringType

visitDef :: MonadSemantic m => Syntax.Def -> m (Typed SemBox)
visitDef (Syntax.Def name expr) = do
    e <- visitExpr expr
    scope %= HM.insert name (Term (typedTy e) HM.empty)
    pure $ tymap (SemBox . Def name) e

visitForeign :: MonadSemantic m => Syntax.Foreign -> m (Typed SemBox)
visitForeign (Syntax.Foreign (Syntax.Sig name atom)) = do
    a <- visitAtom atom
    scope %= HM.insert name (Term (SemType a) HM.empty) -- XXX: need scopes
    pure $ SemBox (Foreign name) `As` SemType a

visitFunction :: MonadSemantic m => Syntax.Function -> m (Typed SemBox)
visitFunction (Syntax.Function (Syntax.ParamList params) expr) = do
    args <- traverse visitParam params
    e <- visitExpr expr
    let paramTys = typedTy <$> args
    pure $ SemBox (Function (ParamList args) e) `As` FunctionType paramTys (typedTy e)

visitCall :: MonadSemantic m => Syntax.Call -> m (Typed SemBox)
visitCall (Syntax.Call callable expr) = do
    c <- visitCompound callable
    e <- visitExpr expr
    ct <- visitCallType (typedTy c)
    pure $ SemBox (Call c e) `As` ct

visitCallType :: MonadSemantic m => Type -> m Type
visitCallType (FunctionType _ retTy)              = pure retTy
visitCallType (BuiltinType FunctionBuiltin)       = pure (BuiltinType FunctionBuiltin)
visitCallType (BuiltinType (Function2Builtin ty)) = pure (BuiltinType (Function2Builtin ty))
visitCallType (BuiltinType IntNBuiltin)           = pure (BuiltinType IntNBuiltin)
visitCallType (BuiltinType PtrBuiltin)            = pure (BuiltinType PtrBuiltin)
visitCallType UnresolvedType                      = pure (UnresolvedType)
visitCallType ty                                  = throwError $ ErrorContext "type is not callable" [ppShow ty]

visitBranch :: MonadSemantic m => Syntax.Branch -> m (Typed SemBox)
visitBranch (Syntax.Branch cond trueBranch falseBranch) = do
    c <- visitCompound cond
    t <- visitExpr trueBranch
    f <- visitExpr falseBranch
    pure $ SemBox (Branch c t f) `As` typedTy t

visitOp :: MonadSemantic m => Syntax.Op -> m (Typed SemBox)
visitOp (Syntax.Op op lhs rhs) = do
    l <- visitAtom lhs
    r <- visitCompound rhs
    pure $ SemBox (Op op l r) `As` typedTy l

visitBlock :: MonadSemantic m => Syntax.Block -> m (Typed SemBox)
visitBlock (Syntax.Block statements) = do
    s <- traverse visitStatement statements
    let retTy = if null s then TupleType [] else typedTy (last s)
    pure $ SemBox (Block s) `As` retTy

visitParam :: MonadSemantic m => Syntax.Param -> m (Typed Param)
visitParam (Syntax.SigParam (Syntax.Sig name atom)) = do
    a <- visitAtom atom
    scope %= HM.insert name (Term (typedTy a) HM.empty) -- XXX: need scopes
    pure (name `As` SemType a)
-- visitParam (Syntax.SigParam (Syntax.AtomSig name ty)) = visitAtom >>= \case
--     TypePrimary t `As` _ ->

--        TypePrimary (expectedTy `As` _) `As` _ -> if actualTy == expectedTy || actualTy == UnresolvedType
--            then pure (pVal `As` expectedTy)
--            else fail ("expected type `" ++ show expectedTy ++ "` does not match actual type `" ++ show actualTy ++ "`: " ++ show sig)
visitParam (Syntax.InferredParam name)            = pure (name `As` UnresolvedType)
