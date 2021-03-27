module Recipes.Backend.Main where

import Backend.Prelude

import Data.Argonaut as Json
import HTTPure as HTTPure
import Node.Encoding (Encoding(..))
import Recipes.API (TestValue, RecipesValue, recipesRoute, testRoute)
import Recipes.Backend.DB (connection, execQuery, recipe, recipeIngredients)
import Recipes.Backend.ServerSetup (loadEnv, logMiddleware, serverOptions)
import Selda (selectFrom)

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

    rtr route | route == testRoute = do
      contents <- encodeJson <$> butterChickenIngredients
      HTTPure.ok $ Json.stringify contents

    rtr route | route == recipesRoute = do
      contents <- encodeJson <$> recipes 
      HTTPure.ok $ Json.stringify contents

    rtr _ = HTTPure.notFound
    
    errHandler err = HTTPure.internalServerError $ show err
  
butterChickenIngredients :: Aff TestValue
butterChickenIngredients = do 
  conn <- liftEffect connection
  execQuery conn $ selectFrom recipeIngredients pure

recipes :: Aff RecipesValue
recipes = do
  conn <- liftEffect connection
  ( execQuery conn $ selectFrom recipe (\{name} -> pure {name})) <##> _.name

main :: Effect Unit
main = launchAff_ do
  loadEnv
  liftEffect $ do 
    config <- serverOptions
    mode <- lookupEnv "MODE"
    let 
      startupSuffix = maybe "" (\m -> i " in "m" mode") mode 
      startupMsg = i "starting server: "config.opts.hostname":"(show config.opts.port)"/"startupSuffix
    void $ HTTPure.serve' config.opts (logMiddleware (router config.dist)) $ log startupMsg
  
