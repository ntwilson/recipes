module Recipes.ErrorHandling where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Exception (Error, error)

-- | `launchAff` and `forkAff` are fire-&-forget, meaning that any errors that arise when running the Aff are ignored.
-- | These functions should _generally_ be avoided, as we would prefer to at a minimum log the error to the console
-- | if an error arises so that it's easier to debug.  This function is a replacement for `launchAff` or `forkAff` where
-- | a handler is provided in the event of an error.
launchAffWithHandler :: forall a. (Error -> Effect Unit) -> Aff a -> Effect Unit
launchAffWithHandler handler aff = 
  aff # runAff_ (case _ of
    Left err -> handler err
    _ -> pure unit)

-- | When propagating errors, it's common that different monads work with different error types.  For example, 
-- | `Effect` and `Aff` work with the `Error` type from Effect.Exception, while for `Either` you can work with
-- | any error type. By constraining the error type to an instance of `Throwable`, you can work with a polymorphic error
-- | type but still be able to create new errors given any showable thing.
class (Show a) <= Throwable a b where fromThrowable :: a -> b
instance errorFromThrowable :: Show a => Throwable a Error where fromThrowable = error <<< show
else instance unitFromThrowable :: Show a => Throwable a Unit where fromThrowable _ = unit
else instance anythingElseFromThrowable :: Show a => Throwable a a where fromThrowable = identity

-- | same as `fromThrowable`, but constrained to work with `String` as a more readable alias and potentially fewer type annotations
fromMessage :: forall e. Throwable String e => String -> e
fromMessage = fromThrowable

-- | A class for polymorphic error propagation. There are multiple monads that propagate errors in the "typical" sense 
-- | (letting them terminate the rest of the function early and return the error to the caller). The `Either`,
-- | `Maybe`, `Effect`, and `Aff` monads all propagate errors in this way. By making a function return a monad of this
-- | class, the specific monad used is determined by the caller. If run from an `Either` monad, it'll return a `Left` 
-- | in the event of an error.  If run from an `Aff` monad, it'll throw an `Error` in the event of an error.
-- | To raise an error in this monad, you can use `throwError $ fromThrowable yourErr`. 
-- | There are three type variables.  The first is the type of error thrown by the function.  The second
-- | is the monad that will propagate the error (probably `Either`, `Maybe`, `Aff`, or `Effect`).  Since not
-- | all of these monads support any type of error, the third is the type of error that gets propagated,
-- | and any thrown error is immediately converted to the propagated type.
-- | When `Either` is the monad, the thrown type and the propagated type are the same.  But when `Maybe` is 
-- | the monad, then the propagated type must be `Unit`.  When `Aff` or `Effect` is the monad, then the propagated
-- | type must be `Error`. Note the type of error propagated is the only type that can be caught, so 
-- | the monad must be `Either` if you want to catch a custom error type.
-- | (This class is just a combination of `MonadError` and `Throwable`. Any type will have a `Throws` instance as long
-- | as it has instances for the other two)
class (MonadError prop monad, Throwable thrown prop) <= Throws thrown monad prop | monad -> prop
instance throws :: (MonadError prop monad, Throwable thrown prop) => Throws thrown monad prop

-- | Take a concrete Either and lift it into your monad of choice.
liftError :: forall monad prop thrown a. Throws thrown monad prop => Either thrown a -> monad a
liftError (Left t) = throwError $ fromThrowable t
liftError (Right a) = pure a


