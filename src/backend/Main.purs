module Recipes.Backend.Main where

import Backend.Prelude

import Control.Monad.Except (runExceptT)
import Recipes.Backend.DB (setupDatabase)
import Recipes.Backend.ServerSetup (loadEnv, logMiddleware, serverOptions)

main :: Effect Unit
main = launchAff_ do
  loadEnv
  runExceptT setupDatabase >>= case _ of 
    Left err -> log err
    Right _ -> pure unit

  -- config <- serverOptions
  -- mode <- env "MODE"
  -- let 
  --   startupSuffix = caseMaybe {nothing: "", just: \m -> i " in "m" mode"} mode 
  --   startupMsg = i "starting server: "config.opts.hostname":"(show config.opts.port)"/"startupSuffix
  -- serve config.opts (logMiddleware (router config.dist)) $ log startupMsg

  -- where 
  --   env = liftEffect <<< lookupEnv
  --   serve options middleware startupAction = 
  --     liftEffect $ void $ HTTPure.serve' options middleware startupAction
  
