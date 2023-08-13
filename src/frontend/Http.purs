module Recipes.Frontend.Http where

import Frontend.Prelude

import Affjax (Request, Response)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, values)

class BodyType a where bodyStr :: Response a -> String
instance BodyType String where bodyStr resp = resp.body
else instance BodyType a where bodyStr _ = ""


runRequest :: ∀ a r. BodyType a => Request a -> Run (AFFECT + EXCEPT String + r) (Response a)
runRequest r = request r # handleErrors # withExcept printError

expectRequest :: ∀ a. BodyType a => Request a -> Aff Unit
expectRequest = runBaseAff' <<< catch log <<< expectRequest'

expectRequest' :: ∀ a r. BodyType a => Request a -> Run (AFFECT + EXCEPT String + r) Unit
expectRequest' rqst = do
  resp <- request rqst # liftAff >>= rethrow # withExcept printError
  when (not $ between 200 299 $ unwrap resp.status) $
    throw (i"status "(show $ unwrap resp.status)". "(bodyStr resp) :: String)

createQueryParameters :: String -> Map String String -> String
createQueryParameters url keyPairs =
  let 
    params = keyPairs # mapWithIndex (\key val -> key <> "=" <> val)
    reducedQuery = intercalate "&" (values params)
  in

  url <> "?" <> reducedQuery
