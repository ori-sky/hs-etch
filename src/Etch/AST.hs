module Etch.AST where

data AST = IntegerLiteral Integer
         | Definition AST AST
           deriving Show
