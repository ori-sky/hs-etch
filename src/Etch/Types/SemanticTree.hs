module Etch.Types.SemanticTree where

import Data.Text (Text)
import qualified Etch.Types.SyntaxTree as Syntax (Type(..))

data Type = FunctionType [Type] Type
          | TupleType [Type]
          | IdentType
          | IntType
          | StringType
          | UnitType
            deriving Show

fromSyntaxType :: Syntax.Type -> Type
fromSyntaxType Syntax.IntType = IntType

data Typed a = As { typedVal :: a
                  , typedTy  :: Type
                  } deriving Show

tymap :: (Typed a -> b) -> Typed a -> Typed b
tymap f typed = f typed `As` typedTy typed

data Statement = DefStatement (Typed Def)
               | ExprStatement (Typed Expr)
                 deriving Show

data Expr = CallExpr (Typed Call)
          | BranchExpr (Typed Branch)
          | CompoundExpr (Typed Compound)
            deriving Show

data Compound = OpCompound (Typed Op)
              | PrimaryCompound (Typed Primary)
                deriving Show

data Primary = BlockPrimary (Typed Block)
             | TuplePrimary [Typed Expr]
             | IdentPrimary Text
             | IntegerPrimary Integer
             | StringPrimary Text
               deriving Show

data Def = Def Text (Typed Expr)
           deriving Show

data Call = Call (Typed Compound) (Typed Expr)
            deriving Show

data Branch = Branch (Typed Compound) (Typed Expr) (Typed Expr)
              deriving Show

data Op = Op Text (Typed Primary) (Typed Compound)
          deriving Show

data Block = Block ParamList [Typed Statement]
             deriving Show

data ParamList = ParamList [Typed Text]
                 deriving Show
