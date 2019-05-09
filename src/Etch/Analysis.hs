{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Etch.Analysis where

import Etch.Types.Semantic

runAnalysis :: MonadAnalysis m => Analysis m -> [Typed SemBox] -> m [Typed SemBox]
runAnalysis ana sems = traverse (visit ana) sems
