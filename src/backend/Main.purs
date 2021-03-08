module Recipes.Backend.Main where

import Prelude

import Data.Foldable (intercalate)
import Data.Int (fromString)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)
import HTTPure as HTTPure
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (lookupEnv)

router :: HTTPure.Request -> HTTPure.ResponseM
router {path: []} = do
  contents <- readTextFile UTF8 "./release/dist/index.html"
  HTTPure.ok' (HTTPure.header "Content-Type" "text/html; charset=UTF-8") contents
router {path: ["main.js"]} = do
  contents <- readTextFile UTF8 "./release/dist/main.js" 
  HTTPure.ok' (HTTPure.header "Content-Type" "text/javascript") contents
router _ = HTTPure.notFound
  
logMiddleware :: (HTTPure.Request -> HTTPure.ResponseM) -> HTTPure.Request -> HTTPure.ResponseM
logMiddleware handler req = do
  log $ i "["(show req.method)"]"(intercalate "/" req.path)
  handler req
  
main :: Effect Unit
main = do
  portStr <- lookupEnv "PORT"
  mode <- lookupEnv "NODE_ENV"
  let 
    hostname = if (mode == Just "production") then "0.0.0.0" else "localhost"
    port = fromMaybe 80 (fromString =<< portStr)
    opts = serverOptions hostname port
    startupMsg = i "starting server: "opts.hostname":"(show port)"/"
  void $ HTTPure.serve' (opts) (logMiddleware router) $ log startupMsg 
  
  where 
    serverOptions hostname port = {hostname, port, backlog: Nothing} 
