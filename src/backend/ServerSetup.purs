module Recipes.Backend.ServerSetup where

import Backend.Prelude

import Data.Foldable (intercalate)
import Data.Int as Int
import Dotenv (loadFile)
import HTTPure as HTTPure
import Node.HTTP as HTTP

loadEnv :: Aff Unit
loadEnv = void loadFile

logMiddleware :: (HTTPure.Request -> HTTPure.ResponseM) -> HTTPure.Request -> HTTPure.ResponseM
logMiddleware handler req = do
  log $ i "["(show req.method)"] /"(intercalate "/" req.path)
  handler req

serverOptions :: ∀ eff. MonadEffect eff => eff {opts :: HTTP.ListenOptions, dist :: String}
serverOptions = do
  portStr <- env "PORT"
  hostEnv <- env "HOST"
  mode <- env "MODE"
  let 
    hostname = fromMaybe "0.0.0.0" hostEnv
    port = fromMaybe 80 (Int.fromString =<< portStr)
    dist = "./dist" -- if mode == Just "development" then "./dist" else "./release/dist"
  pure {opts: {hostname, port, backlog: Nothing}, dist} 
  
  where  
    env = liftEffect <<< lookupEnv
  
