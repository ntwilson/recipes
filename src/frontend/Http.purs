module Recipes.Frontend.Http where

import Frontend.Prelude

import Affjax (Request, Response)
import Control.Monad.Except (ExceptT(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, values)

class BodyType a where bodyStr :: Response a -> String
instance BodyType String where bodyStr resp = resp.body
else instance BodyType a where bodyStr _ = ""


type HTTP_ERROR r = (httpError :: String | r)
httpError :: ∀ r. String -> Variant (HTTP_ERROR r)
httpError = inj @"httpError"

runRequest :: ∀ m a r. MonadAff m => BodyType a => Request a -> ExceptV (HTTP_ERROR + r) m (Response a)
runRequest r = request r <#> lmap (printError >>> httpError) # liftAff # ExceptT

expectRequest :: ∀ m a r. MonadAff m => BodyType a => Request a -> ExceptV (HTTP_ERROR + r) m Unit
expectRequest rqst = do
  resp <- request rqst # liftAff <#> lmap (printError >>> httpError) # ExceptT
  when (not $ between 200 299 $ unwrap resp.status) $
    throwError (httpError $ i"status "(show $ unwrap resp.status)". "(bodyStr resp))

createQueryParameters :: String -> Map String String -> String
createQueryParameters url keyPairs =
  let 
    params = keyPairs # mapWithIndex (\key val -> key <> "=" <> val)
    reducedQuery = intercalate "&" (values params)
  in

  url <> "?" <> reducedQuery
