module Recipes.Backend.Main where

import Backend.Prelude

import HTTPurple (Request, ResponseM)
import HTTPurple as HTTPure
import Recipes.API (RecipeRoute, recipeRouteDuplex)
import Recipes.Backend.Routing as Routing
import Recipes.Backend.ServerSetup (loadEnv, logMiddleware, serverOptions)

main :: Effect Unit
main = launchAff_ do
  loadEnv
  mode <- env "MODE"
  config <- serverOptions mode
  serve config.opts (logMiddleware (Routing.run config.dist))
  pure unit

  where 
    env = liftEffect <<< lookupEnv
    serve :: {hostname::String, port::Int, onStarted::Effect Unit} -> (Request RecipeRoute -> ResponseM) -> Aff Unit
    serve options router = 
      liftEffect $ void $ HTTPure.serve options { route: recipeRouteDuplex, router }

  
