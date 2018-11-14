module Etch.AST where

import Data.Text

data Def = Def Text Expr
           deriving Show

data Expr = BranchExpr Branch
          | CompoundExpr Compound
            deriving Show

data Compound = OpCompound Op
              | PrimaryCompound Primary
                deriving Show

data Primary = BlockPrimary Block
             | TuplePrimary [Expr]
             | IdentPrimary Text
             | IntegerPrimary Integer
             | StringPrimary Text
               deriving Show

data Branch = Branch Compound Expr Expr
              deriving Show

data Op = Op Text Primary Compound
          deriving Show

data Block = Block [Text] [Expr]
             deriving Show
