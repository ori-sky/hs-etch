module Etch.Types.Module where

import Etch.Types.SemanticTree (Def)

data Module = Module { moduleName       :: String
                     , moduleSourceFile :: String
                     , moduleDefs       :: [Def]
                     }

defaultModule :: String -> [Def] -> Module
defaultModule srcFile defs = Module { moduleName       = srcFile
                                    , moduleSourceFile = srcFile
                                    , moduleDefs       = defs
                                    }
