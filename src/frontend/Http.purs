module Recipes.Frontend.Http where

import Frontend.Prelude

import Affjax (Request, Response)

class BodyType a where bodyStr :: Response a -> String
instance strBody :: BodyType String where bodyStr resp = resp.body
else instance otherBody :: BodyType a where bodyStr _ = ""

expectRequest :: ∀ a. BodyType a => Request a -> Aff Unit
expectRequest rqst = do
  tryResp <- request rqst
  resp <- tryResp # liftErrorVia printError
  if between 200 299 $ unwrap resp.status
  then pure unit
  else throw (i"status "(show $ unwrap resp.status)". "(bodyStr resp) :: String)
