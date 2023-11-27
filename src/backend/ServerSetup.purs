module Recipes.Backend.ServerSetup where

import Backend.Prelude

import Data.Foldable (intercalate)
import Data.Int as Int
import Dotenv (loadFile)
import HTTPurple as HTTPure

loadEnv :: Aff Unit
loadEnv = void loadFile

logMiddleware :: ∀ r. (HTTPure.Request r -> HTTPure.ResponseM) -> HTTPure.Request r -> HTTPure.ResponseM
logMiddleware handler req = do
  log $ i "["(show req.method)"] /"(intercalate "/" req.path)
  handler req

serverOptions :: ∀ eff. MonadEffect eff => Maybe String -> eff {opts::{hostname::String, port::Int, onStarted::Effect Unit}, dist::String}
serverOptions mode = do
  portStr <- env "PORT"
  hostEnv <- env "HOST"
  let 
    hostname = fromMaybe "0.0.0.0" hostEnv
    port = fromMaybe 80 (Int.fromString =<< portStr)
    startupSuffix = caseMaybe {nothing: "", just: \m -> i" in "m" mode"} mode 
    startupMsg :: String
    startupMsg = i "starting server: "hostname":"port"/"startupSuffix
    opts = {hostname, port, onStarted: log startupMsg}

  pure {opts, dist: "./dist"}
  
  where
    env = liftEffect <<< lookupEnv
  
