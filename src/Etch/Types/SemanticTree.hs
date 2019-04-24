module Etch.Types.SemanticTree where

import Data.Text (Text)

data Type = TupleType [Type]
          | FunctionType [Type] Type
          | IntType Integer
          | StringType
          | NewType Integer [Type]
          | PrimaryType (Typed Primary)
          | BuiltinType Builtin
          | UnresolvedType
            deriving (Eq, Show)

data Typed a = a `As` Type
               deriving (Eq, Show)

typedVal :: Typed a -> a
typedVal (x `As` _) = x

typedTy :: Typed a -> Type
typedTy (_ `As` t) = t

tymap :: (Typed a -> b) -> Typed a -> Typed b
tymap f typed = f typed `As` typedTy typed

data Statement = DefStatement (Typed Def)
               | ExprStatement (Typed Expr)
                 deriving (Eq, Show)

data Expr = FunctionExpr (Typed Function)
          | CallExpr (Typed Call)
          | BranchExpr (Typed Branch)
          | CompoundExpr (Typed Compound)
            deriving (Eq, Show)

data Compound = OpCompound (Typed Op)
              | PrimaryCompound (Typed Primary)
                deriving (Eq, Show)

data Primary = BlockPrimary (Typed Block)
             | TypePrimary (Typed Type)
             | TuplePrimary [Typed Expr]
             | NewPrimary Integer [Typed Expr]
             | IdentPrimary Text
             | IntegerPrimary Integer
             | StringPrimary Text
             | BuiltinPrimary Builtin
               deriving (Eq, Show)

data Builtin = IntNBuiltin
             | SizedIntBuiltin Integer
             | StringBuiltin
               deriving (Eq, Show)

data Def = Def Text (Typed Expr)
           deriving (Eq, Show)

data Function = Function ParamList (Typed Expr)
                deriving (Eq, Show)

data Call = Call (Typed Compound) (Typed Expr)
            deriving (Eq, Show)

data Branch = Branch (Typed Compound) (Typed Expr) (Typed Expr)
              deriving (Eq, Show)

data Op = Op Text (Typed Primary) (Typed Compound)
          deriving (Eq, Show)

data Block = Block [Typed Statement]
             deriving (Eq, Show)

data ParamList = ParamList [Typed Param]
                 deriving (Eq, Show)

type Param = Text
