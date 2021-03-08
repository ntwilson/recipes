module Recipes.Backend.Main where

import Prelude

import Data.Foldable (intercalate)
import Effect (Effect)
import Effect.Class.Console (log)
import HTTPure as HTTPure
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

router :: HTTPure.Request -> HTTPure.ResponseM
router {path: []} = do
  contents <- readTextFile UTF8 "./dist/index.html"
  HTTPure.ok' (HTTPure.header "Content-Type" "text/html; charset=UTF-8") contents
router {path: ["main.js"]} = do
  contents <- readTextFile UTF8 "./dist/main.js" 
  HTTPure.ok' (HTTPure.header "Content-Type" "text/javascript") contents
router _ = HTTPure.notFound
  
logMiddleware :: (HTTPure.Request -> HTTPure.ResponseM) -> HTTPure.Request -> HTTPure.ResponseM
logMiddleware handler req = do
  log $ "[" <> show req.method <> "]" <> intercalate "/" req.path
  handler req
  
main :: Effect Unit
main = void $ HTTPure.serve 8080 (logMiddleware router) $ log "starting server"
