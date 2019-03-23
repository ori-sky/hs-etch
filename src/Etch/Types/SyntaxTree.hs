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
              | AtomCompound Atom
                deriving Show

data Atom = SigAtom (Sig Primary)
          | PrimaryAtom Primary
            deriving Show

data Primary = BlockPrimary Block
             | TypePrimary Type
             | TuplePrimary [Expr]
             | IdentPrimary Text
             | IntegerPrimary Integer
             | StringPrimary Text
               deriving Show

data Sig a = Sig a Type
           | AtomSig a Atom
             deriving Show

data Type = IntType Integer
          | NewType ParamList [Atom]
            deriving Show

data Def = Def Text Expr
           deriving Show

data Call = Call Compound Expr
            deriving Show

data Branch = Branch Compound Expr Expr
              deriving Show

data Op = Op Text Atom Compound
          deriving Show

data Block = Block ParamList [Statement]
             deriving Show

data ParamList = ParamList [Param]
                 deriving Show

data Param = SigParam (Sig Text)
           | InferredParam Text
             deriving Show
