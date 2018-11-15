{-# LANGUAGE FlexibleContexts #-}

module Etch.CodeGen.Context where

import qualified Data.HashMap.Lazy as HM
import Data.Maybe (maybe)
import Data.Text (Text)
import Control.Monad.State

type Scope a = HM.HashMap Text a

data Context a = Context { contextScopes :: [Scope a]
                         , contextNextID :: Integer
                         }

defaultContext :: Context a
defaultContext = Context { contextScopes = [HM.empty]
                         , contextNextID = 0
                         }

contextLookup :: Text -> Context a -> Maybe a
contextLookup name Context { contextScopes = scopes } = f scopes
  where f (scope:xs) = maybe (f xs) pure (HM.lookup name scope)
        f []         = Nothing

contextInsert :: Text -> a -> Context a -> Context a
contextInsert name value ctx@(Context { contextScopes = (scope:xs) }) =
    ctx { contextScopes = HM.insert name value scope : xs }
contextInsert _ _ ctx = ctx

contextInsertScope :: Scope a -> Context a -> Context a
contextInsertScope scope ctx@(Context { contextScopes = scopes }) =
    ctx { contextScopes = scope : scopes }

contextStateNextID :: MonadState (Context a) m => m Integer
contextStateNextID = do
    ctx@(Context { contextNextID = nextID }) <- get
    put ctx { contextNextID = succ nextID }
    pure nextID
