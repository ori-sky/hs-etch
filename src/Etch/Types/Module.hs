module Etch.Types.Module where

import Etch.Types.SemanticTree (Typed, Def)

data Module = Module { moduleName       :: String
                     , moduleSourceFile :: String
                     , moduleDefs       :: [Typed Def]
                     }

defaultModule :: String -> [Typed Def] -> Module
defaultModule srcFile defs = Module { moduleName       = srcFile
                                    , moduleSourceFile = srcFile
                                    , moduleDefs       = defs
                                    }
