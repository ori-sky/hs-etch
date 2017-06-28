module Etch.AST where

import Data.ByteString.Char8 (ByteString)

data AST = Call ByteString AST
         | Block [AST]
         | Tuple [AST]
         | Identifier ByteString
         | IntegerLiteral Integer
         | StringLiteral ByteString
           deriving Show
