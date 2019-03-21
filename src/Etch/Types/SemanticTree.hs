module Etch.Types.SemanticTree where

import Data.Text (Text)

data Type = FunctionType [Type] Type
          | TupleType [Type]
          | IntType Integer
          | StringType
          | UnitType
          | NewType [Typed Expr]
          | KindType Kind
          | UnresolvedType
            deriving (Eq, Show)

data Kind = TypeKind
            deriving (Eq, Show)

data Typed a = As { typedVal :: a
                  , typedTy  :: Type
                  } deriving (Eq, Show)

tymap :: (Typed a -> b) -> Typed a -> Typed b
tymap f typed = f typed `As` typedTy typed

data Statement = DefStatement (Typed Def)
               | ExprStatement (Typed Expr)
                 deriving (Eq, Show)

data Expr = CallExpr (Typed Call)
          | BranchExpr (Typed Branch)
          | CompoundExpr (Typed Compound)
            deriving (Eq, Show)

data Compound = OpCompound (Typed Op)
              | PrimaryCompound (Typed Primary)
                deriving (Eq, Show)

data Primary = BlockPrimary (Typed Block)
             | TypePrimary (Typed Type)
             | TuplePrimary [Typed Expr]
             | IdentPrimary Text
             | IntegerPrimary Integer
             | StringPrimary Text
               deriving (Eq, Show)

data Def = Def Text (Typed Expr)
           deriving (Eq, Show)

data Call = Call (Typed Compound) (Typed Expr)
            deriving (Eq, Show)

data Branch = Branch (Typed Compound) (Typed Expr) (Typed Expr)
              deriving (Eq, Show)

data Op = Op Text (Typed Primary) (Typed Compound)
          deriving (Eq, Show)

data Block = Block ParamList [Typed Statement]
             deriving (Eq, Show)

data ParamList = ParamList [Typed Param]
                 deriving (Eq, Show)

type Param = Text
