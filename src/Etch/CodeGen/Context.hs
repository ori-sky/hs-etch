module Etch.CodeGen.Context where

import qualified Data.HashMap.Lazy as HM
import Data.Maybe (maybe)
import Data.Text (Text)

type Scope a = HM.HashMap Text a

data Context a = Context { contextScopes :: [Scope a]
                         }

defaultContext :: Context a
defaultContext = Context { contextScopes = [HM.empty]
                         }

contextLookup :: Text -> Context a -> Maybe a
contextLookup name Context { contextScopes = scopes } = f scopes
  where f (scope:xs) = maybe (f xs) pure (HM.lookup name scope)
        f []         = Nothing

contextInsert :: Text -> a -> Context a -> Context a
contextInsert name value Context { contextScopes = (scope:xs) } =
    Context { contextScopes = HM.insert name value scope : xs }
contextInsert _ _ ctx = ctx

contextInsertScope :: Scope a -> Context a -> Context a
contextInsertScope scope Context { contextScopes = scopes } =
    Context { contextScopes = scope : scopes }
