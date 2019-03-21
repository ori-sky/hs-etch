module Etch.Types.SyntaxTree where

import Data.Text (Text)

data Statement = DefStatement Def
               | ExprStatement Expr
                 deriving Show

data Expr = CallExpr Call
          | BranchExpr Branch
          | CompoundExpr Compound
            deriving Show

data Compound = OpCompound Op
              | SigCompound (Sig Primary)
              | PrimaryCompound Primary
                deriving Show

data Primary = BlockPrimary Block
             | TuplePrimary [Expr]
             | IdentPrimary Text
             | IntegerPrimary Integer
             | StringPrimary Text
               deriving Show

data Sig a = Sig a Type
             deriving Show

data Type = IntType Integer
          | InferredType
            deriving Show

data Def = Def Text Expr
           deriving Show

data Call = Call Compound Expr
            deriving Show

data Branch = Branch Compound Expr Expr
              deriving Show

data Op = Op Text Primary Compound
          deriving Show

data Block = Block ParamList [Statement]
             deriving Show

data ParamList = ParamList [Sig Text]
                 deriving Show
