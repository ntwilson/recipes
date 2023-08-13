module Recipes.Backend.Main where

import Backend.Prelude

import HTTPure (Request, ResponseM)
import HTTPure as HTTPure
import Node.HTTP (ListenOptions)
import Recipes.Backend.Routing as Routing
import Recipes.Backend.ServerSetup (loadEnv, logMiddleware, serverOptions)

main :: Effect Unit
main = launchAff_ do
  loadEnv
  config <- serverOptions
  mode <- env "MODE"
  let 
    startupSuffix = caseMaybe {nothing: "", just: \m -> i" in "m" mode"} mode 
    startupMsg :: String
    startupMsg = i "starting server: "config.opts.hostname":"config.opts.port"/"startupSuffix
  serve config.opts (logMiddleware (Routing.run config.dist)) $ log startupMsg
  pure unit

  where 
    env = liftEffect <<< lookupEnv
    serve :: ListenOptions -> (Request -> ResponseM) -> Effect Unit -> Aff Unit
    serve options middleware startupAction = 
      liftEffect $ void $ HTTPure.serve' options middleware startupAction
  
