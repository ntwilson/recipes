module Recipes.Backend.ServerSetup where

import Backend.Prelude

import Data.Foldable (intercalate)
import Data.Int as Int
import Dotenv (loadFile)
import HTTPure as HTTPure

loadEnv :: Aff Unit
loadEnv = void loadFile

logMiddleware :: (HTTPure.Request -> HTTPure.ResponseM) -> HTTPure.Request -> HTTPure.ResponseM
logMiddleware handler req = do
  log $ i "["(show req.method)"] /"(intercalate "/" req.path)
  handler req

serverOptions :: Effect {hostname :: String, port :: Int, backlog :: Maybe Int}
serverOptions = do
  portStr <- lookupEnv "PORT"
  hostEnv <- lookupEnv "HOST"
  let 
    hostname = fromMaybe "0.0.0.0" hostEnv
    port = fromMaybe 80 (Int.fromString =<< portStr)
  pure {hostname, port, backlog: Nothing} 
  
