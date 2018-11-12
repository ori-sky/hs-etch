module Etch.Module where

import Etch.AST

data Module = Module { moduleName        :: String
                     , moduleSourceFile  :: String
                     , moduleDefinitions :: [Def]
                     }

defaultModule :: String -> [Def] -> Module
defaultModule srcFile defs = Module { moduleName        = srcFile
                                    , moduleSourceFile  = srcFile
                                    , moduleDefinitions = defs
                                    }
