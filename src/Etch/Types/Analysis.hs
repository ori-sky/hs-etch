{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Etch.Types.Analysis where

import qualified Data.HashMap.Lazy as HM
import Data.Text
import Control.Monad.Except
import Control.Monad.State
import Etch.Types.ErrorContext
import Etch.Types.SemanticTree

type Scope = HM.HashMap Text Term

data Term = Term Type Scope
            deriving Show

data AnalysisState = AnalysisState { _analysisStateNextID :: Integer
                                   , _analysisStateScope  :: Scope
                                   } deriving Show

defaultAnalysisState :: AnalysisState
defaultAnalysisState = AnalysisState { _analysisStateNextID = 1
                                     , _analysisStateScope  = HM.empty
                                     }

type MonadAnalysis m = (MonadError ErrorContext m, MonadState AnalysisState m)
