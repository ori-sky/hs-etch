{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Etch.Types.Semantic where

import qualified Data.HashMap.Lazy as HM
import Data.Text (Text)
import Control.Monad.Except
import Control.Monad.State
import Text.Show.Pretty (ppShow)
import Etch.Types.ErrorContext

class Show a => Semantic a where
    visit :: MonadAnalysis m => Analysis m -> Typed a -> m (Typed SemBox)
    foldConstant :: MonadAnalysis m => Typed a -> m (Maybe Integer)

data SemBox = forall a. Semantic a => SemBox a

instance Show SemBox where show (SemBox x) = show x

data Type = TupleType [Type]
          | FunctionType [Type] Type
          | PtrType Type
          | IntType Integer
          | StringType
          | NewType Integer [Type]
          | BuiltinType Builtin
          | SemType (Typed SemBox)
          | UnresolvedType
            deriving Show

data Typed a = a `As` Type
               deriving Show

typedVal :: Typed a -> a
typedVal (x `As` _) = x

typedTy :: Typed a -> Type
typedTy (_ `As` t) = t

tymap :: (Typed a -> b) -> Typed a -> Typed b
tymap f typed = f typed `As` typedTy typed

data Primary = IdentPrimary Text
             | IntegerPrimary Integer
             | StringPrimary Text
             | BuiltinPrimary Builtin
               deriving Show

data Builtin = FunctionBuiltin
             | Function2Builtin Type
             | IntNBuiltin
             | PtrBuiltin
             | FunctionTypeBuiltin Type Type
             | IntTypeBuiltin Integer
             | PtrTypeBuiltin Type
             | StringBuiltin
               deriving Show

data Def = Def Text (Typed SemBox)
           deriving Show

data Foreign = Foreign Text
               deriving Show

data Function = Function ParamList (Typed SemBox)
                deriving Show

data Call = Call (Typed SemBox) (Typed SemBox)
            deriving Show

data Branch = Branch (Typed SemBox) (Typed SemBox) (Typed SemBox)
              deriving Show

data Op = Op Text (Typed SemBox) (Typed SemBox)
          deriving Show

data Block = Block [Typed SemBox]
             deriving Show

data Tuple = Tuple [Typed SemBox]
             deriving Show

data New = New Integer [Typed SemBox]
           deriving Show

data ParamList = ParamList [Typed Param]
                 deriving Show

type Param = Text

instance Semantic SemBox where
    visit ana    (SemBox x `As` ty) = visit ana    (x `As` ty)
    foldConstant (SemBox x `As` ty) = foldConstant (x `As` ty)

instance Semantic Block where
    visit ana (Block sems `As` _) = do
        ss <- traverse (visit ana) sems
        let retTy = if null ss then TupleType [] else typedTy (last ss)
        blockAnalysis ana $ Block ss `As` retTy
    foldConstant _ = pure Nothing

instance Semantic Branch where
    visit ana x = branchAnalysis ana x
    foldConstant _ = pure Nothing

instance Semantic Call where
    visit ana (Call callable param `As` _) = do
        c `As` cty <- visit ana callable
        p `As` pty <- visit ana param
        ct <- visitType ana cty
        pt <- visitType ana pty
        retTy <- visitCallType ana ct (p `As` pt)
        case retTy of
            BuiltinType IntNBuiltin -> foldConstant (p `As` pt) >>= \case
                Just n              -> pure $ SemBox (BuiltinPrimary builtin) `As` BuiltinType builtin
                  where builtin = IntTypeBuiltin n
                Nothing             -> throwError $ ErrorContext "intn called with non-constant parameter" [ppShow (p `As` pt)]
            _                       -> callAnalysis ana $ Call (c `As` ct) (p `As` pt) `As` retTy
    foldConstant _ = pure Nothing

instance Semantic Def where
    visit ana (Def name val `As` _) = do
        x `As` ty <- visit ana val
        t <- visitType ana ty
        defAnalysis ana $ tymap (Def name) (x `As` t)
    foldConstant _ = pure Nothing

instance Semantic Foreign where
    visit ana (Foreign name `As` ty) = do
        t <- visitType ana ty
        foreignAnalysis ana (Foreign name `As` t)
    foldConstant _ = pure Nothing

instance Semantic Function where
    visit ana (Function (ParamList params) sem `As` _) = do
        ps <- traverse (visitParam ana) params
        s `As` sty <- visit ana sem
        st <- visitType ana sty
        functionAnalysis ana $ Function (ParamList ps) (s `As` st) `As` FunctionType (fmap typedTy ps) st
    foldConstant _ = pure Nothing

--visitFunction (Function (ParamList params) expr `As` _) = do
--    args <- traverse paramAnalysis params
--    e <- exprAnalysis expr
--    paramTys <- traverse typeAnalysis (typedTy <$> args)
--    pure $ Function (ParamList args) e `As` FunctionType paramTys (typedTy e)

instance Semantic New where
    visit ana x = newAnalysis ana x
    foldConstant _ = pure Nothing

instance Semantic Op where
    visit ana (Op op lhs rhs `As` _) = do
        l `As` lty <- visit ana lhs
        r `As` rty <- visit ana rhs
        lt <- visitType ana lty
        rt <- visitType ana rty
        opAnalysis ana $ Op op (l `As` lt) (r `As` rt) `As` lt
    foldConstant _ = pure Nothing

instance Semantic Primary where
    visit ana x = primaryAnalysis ana x
    foldConstant (IntegerPrimary x `As` _) = pure (Just x)
    foldConstant _                         = pure Nothing

instance Semantic Tuple where
    visit ana (Tuple sems `As` _) = do
        ss <- traverse (visit ana) sems
        tupleAnalysis ana $ Tuple ss `As` TupleType (fmap typedTy ss)
    foldConstant _ = pure Nothing

visitCallType :: MonadAnalysis m => Analysis m -> Type -> Typed SemBox -> m Type
visitCallType ana (FunctionType _ retTy) _              = typeAnalysis ana retTy
visitCallType ana (BuiltinType FunctionBuiltin) sem = do
    t <- SemType <$> visit ana sem
    BuiltinType . Function2Builtin <$> visitType ana t
visitCallType ana (BuiltinType (Function2Builtin ty)) sem = do
    f2t <- visitType ana ty
    t <- SemType <$> visit ana sem
    BuiltinType . FunctionTypeBuiltin f2t <$> visitType ana t
visitCallType _ (BuiltinType IntNBuiltin) _  = pure (BuiltinType IntNBuiltin)
visitCallType _ (BuiltinType PtrBuiltin) _   = pure (BuiltinType PtrBuiltin)
visitCallType _ UnresolvedType _             = pure (UnresolvedType)
visitCallType _ ty e                         = throwError $ ErrorContext "resolved type is not callable" [ppShow ty, ppShow e]

visitParam :: MonadAnalysis m => Analysis m -> Typed Param -> m (Typed Param)
visitParam ana (name `As` ty) = do
    t <- visitType ana ty
    paramAnalysis ana (name `As` t)

visitType :: MonadAnalysis m => Analysis m -> Type -> m Type
visitType ana (FunctionType tys retTy) = do
    t <- FunctionType <$> traverse (visitType ana) tys <*> visitType ana retTy
    typeAnalysis ana t
visitType ana (SemType typed) = do
    t <- SemType <$> visit ana typed
    typeAnalysis ana t
visitType ana ty = typeAnalysis ana ty

-- Analysis types

type Scope = HM.HashMap Text Term

data Term = Term Type Scope
            deriving Show

data AnalysisState = AnalysisState { _analysisStateNextID :: Integer
                                   , _analysisStateScope  :: Scope
                                   } deriving Show

defaultAnalysisState :: AnalysisState
defaultAnalysisState = AnalysisState { _analysisStateNextID = 1
                                     , _analysisStateScope  = HM.empty
                                     }

data Analysis m = Analysis { blockAnalysis    :: Typed Block    -> m (Typed SemBox)
                           , branchAnalysis   :: Typed Branch   -> m (Typed SemBox)
                           , callAnalysis     :: Typed Call     -> m (Typed SemBox)
                           , defAnalysis      :: Typed Def      -> m (Typed SemBox)
                           , foreignAnalysis  :: Typed Foreign  -> m (Typed SemBox)
                           , functionAnalysis :: Typed Function -> m (Typed SemBox)
                           , newAnalysis      :: Typed New      -> m (Typed SemBox)
                           , opAnalysis       :: Typed Op       -> m (Typed SemBox)
                           , paramAnalysis    :: Typed Param    -> m (Typed Param)
                           , primaryAnalysis  :: Typed Primary  -> m (Typed SemBox)
                           , tupleAnalysis    :: Typed Tuple    -> m (Typed SemBox)
                           , typeAnalysis     :: Type           -> m Type
                           }

type MonadSemantic m = (MonadError ErrorContext m, MonadState AnalysisState m)
type MonadAnalysis m = (MonadError ErrorContext m, MonadState AnalysisState m)
