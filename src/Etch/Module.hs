module Etch.Module where

import Etch.AST

data Module = Module { moduleName       :: String
                     , moduleSourceFile :: String
                     , moduleASTs       :: [AST]
                     }

defaultModule :: String -> [AST] -> Module
defaultModule srcFile asts = Module { moduleName       = srcFile
                                    , moduleSourceFile = srcFile
                                    , moduleASTs       = asts
                                    }
