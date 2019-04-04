{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Etch.Analysis.Resolution where

import Control.Monad.Except
import Control.Monad.Reader
import Etch.Types.Analysis
import Etch.Types.ErrorContext
import Etch.Types.SemanticTree

type MonadResolution m = (MonadError ErrorContext m, MonadReader AnalysisState m)

analysis :: MonadResolution m => [Typed Statement] -> m [Typed Statement]
analysis statements = pure statements
