module Recipes.Frontend.Http where

import Frontend.Prelude

import Affjax (Request, Response)
import Data.Map (Map, values)
import Data.FunctorWithIndex (mapWithIndex)

class BodyType a where bodyStr :: Response a -> String
instance BodyType String where bodyStr resp = resp.body
else instance BodyType a where bodyStr _ = ""

expectRequest :: ∀ a. BodyType a => Request a -> Aff Unit
expectRequest rqst = do
  tryResp <- request rqst
  resp <- tryResp # liftErrorVia printError
  if between 200 299 $ unwrap resp.status
  then pure unit
  else throw (i"status "(show $ unwrap resp.status)". "(bodyStr resp) :: String)

createQueryParameters :: String -> Map String String -> String
createQueryParameters url keyPairs =
  let 
    params = keyPairs # mapWithIndex (\key val -> key <> "=" <> val)
    reducedQuery = intercalate "&" (values params)
  in

  url <> "?" <> reducedQuery
