module Etch.Types.Module where

import Etch.Types.SemanticTree (Typed, SemBox)

data Module = Module { moduleName       :: String
                     , moduleSourceFile :: String
                     , moduleDefs       :: [Typed SemBox]
                     }

defaultModule :: String -> [Typed SemBox] -> Module
defaultModule srcFile defs = Module { moduleName       = srcFile
                                    , moduleSourceFile = srcFile
                                    , moduleDefs       = defs
                                    }
