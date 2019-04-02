{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Etch.Types.Lenses where

import Control.Lens.TH (makeFields)
import Etch.Types.Analysis

makeFields ''AnalysisState
