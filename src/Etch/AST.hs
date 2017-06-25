module Etch.AST where

import Data.ByteString.Char8 (ByteString)

data AST = IntegerLiteral Integer
         | Identifier ByteString
         | Definition AST AST
         | Function [AST]
           deriving Show
