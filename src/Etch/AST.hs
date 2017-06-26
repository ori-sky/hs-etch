module Etch.AST where

import Data.ByteString.Char8 (ByteString)

data AST = Definition AST AST
         | Function AST AST
         | Block [AST]
         | Call ByteString AST
         | Tuple [AST]
         | Identifier ByteString
         | IntegerLiteral Integer
           deriving Show
