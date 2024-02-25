module Recipes.ErrorHandling where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Exception (Error)

-- | `launchAff` and `forkAff` are fire-&-forget, meaning that any errors that arise when running the Aff are ignored.
-- | These functions should _generally_ be avoided, as we would prefer to at a minimum log the error to the console
-- | if an error arises so that it's easier to debug.  This function is a replacement for `launchAff` or `forkAff` where
-- | a handler is provided in the event of an error.
launchAffWithHandler :: ∀ a. (Error -> Effect Unit) -> Aff a -> Effect Unit
launchAffWithHandler handler aff = 
  flip runAff_ aff $ case _ of
    Left err -> handler err
    _ -> pure unit

