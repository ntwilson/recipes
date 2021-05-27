module Recipes.Backend.Main where

import Backend.Prelude

import HTTPure as HTTPure
import Recipes.Backend.Routing (router)
import Recipes.Backend.ServerSetup (loadEnv, logMiddleware, serverOptions)

main :: Effect Unit
main = launchAff_ do
  loadEnv
  config <- serverOptions
  mode <- env "MODE"
  let 
    startupSuffix = maybe "" (\m -> i " in "m" mode") mode 
    startupMsg = i "starting server: "config.opts.hostname":"(show config.opts.port)"/"startupSuffix
  serve config.opts (logMiddleware (router config.dist)) $ log startupMsg

  where 
    env = liftEffect <<< lookupEnv
    serve options middleware startupAction = 
      liftEffect $ void $ HTTPure.serve' options middleware startupAction
  
