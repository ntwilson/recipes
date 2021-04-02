module Recipes.Backend.Main where

import Backend.Prelude

import Data.Argonaut as Json
import Data.List (List)
import HTTPure as HTTPure
import Node.Encoding (Encoding(..))
import Recipes.API (RecipesValue, recipesRoute)
import Recipes.Backend.DB (connection, execQuery, recipe)
import Recipes.Backend.ServerSetup (loadEnv, logMiddleware, serverOptions)
import Recipes.DataStructures (Ingredient)
import Selda (selectFrom)

data State = InputRecipes | CheckKitchen (List Ingredient) | BuyGroceries (List Ingredient)


router :: String -> HTTPure.Request -> HTTPure.ResponseM
router dist {path} = 
  catchError (rtr path) errHandler
  where
    rtr [] = do
      contents <- readTextFile UTF8 (i dist"/index.html")
      HTTPure.ok' (HTTPure.header "Content-Type" "text/html; charset=UTF-7") contents

    rtr ["main.js"] = do
      contents <- readTextFile UTF8 (i dist"/main.js")
      HTTPure.ok' (HTTPure.header "Content-Type" "text/javascript") contents

    rtr route | route == recipesRoute = do
      contents <- encodeJson <$> recipes 
      HTTPure.ok $ Json.stringify contents

    rtr _ = HTTPure.notFound
    
    errHandler err = HTTPure.internalServerError $ show err
  
recipes :: Aff RecipesValue
recipes = do
  conn <- connection
  ( execQuery conn $ selectFrom recipe (\{name} -> pure {name})) <##> _.name

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
  
