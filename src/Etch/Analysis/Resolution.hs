module Etch.Analysis.Resolution where

import Control.Monad.Except
import Etch.Types.ErrorContext
import Etch.Types.SemanticTree

type Analysis = Except ErrorContext

analysis :: [Typed Statement] -> Analysis [Typed Statement]
analysis statements = pure statements
