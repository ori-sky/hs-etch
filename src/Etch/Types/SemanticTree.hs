{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Etch.Types.SemanticTree where

import Data.Text (Text)

class Show a => Semantic a where
    visit :: (Monad m, Semantic b) => Typed a -> m (Typed b)

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

instance Semantic Primary  where visit = undefined
instance Semantic Def      where visit = undefined
instance Semantic Foreign  where visit = undefined
instance Semantic Function where visit = undefined
instance Semantic Call     where visit = undefined
instance Semantic Branch   where visit = undefined
instance Semantic Op       where visit = undefined
instance Semantic Block    where visit = undefined
instance Semantic Tuple    where visit = undefined
instance Semantic New      where visit = undefined
