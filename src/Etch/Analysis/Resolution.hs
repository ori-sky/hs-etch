{-# LANGUAGE FlexibleContexts #-}

module Etch.Analysis.Resolution where

import Control.Monad.Except
import Etch.Types.ErrorContext
import Etch.Types.SemanticTree

analysis :: MonadError ErrorContext m => [Typed Statement] -> m [Typed Statement]
analysis statements = pure statements
