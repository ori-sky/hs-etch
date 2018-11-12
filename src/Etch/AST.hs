module Etch.AST where

import Data.Text

data Def = Def Text Expr
           deriving Show

data Expr = OpExpr Op
          | PrimaryExpr Primary
            deriving Show

data Op = Op Text Primary Expr
          deriving Show

data Primary = BlockPrimary Block
             | TuplePrimary [Expr]
             | IdentPrimary Text
             | IntegerPrimary Integer
             | StringPrimary Text
               deriving Show

data Block = Block [Text] [Expr]
             deriving Show
