module Recipes.Backend.Main where

import Backend.Prelude

import Database.PostgreSQL (PGError)
import HTTPure as HTTPure
import Node.Encoding (Encoding(..))
import Recipes.Backend.DB (RecipeIngredients, connection, recipeIngredients)
import Recipes.Backend.ServerSetup (loadEnv, logMiddleware, serverOptions)
import Selda (selectFrom)
import Selda.PG.Aff (query)

router :: String -> HTTPure.Request -> HTTPure.ResponseM
router dist {path: []} = do
  contents <- readTextFile UTF8 (i dist"/index.html")
  HTTPure.ok' (HTTPure.header "Content-Type" "text/html; charset=UTF-8") contents
router dist {path: ["main.js"]} = do
  contents <- readTextFile UTF8 (i dist"/main.js")
  HTTPure.ok' (HTTPure.header "Content-Type" "text/javascript") contents
router _ {path: ["api","test"]} = do
  contents <- show <$> butterChickenIngredients
  log $ i "responding with "contents
  HTTPure.ok contents
router _ _ = HTTPure.notFound
  
butterChickenIngredients :: Aff $ Either PGError (Array $ Record RecipeIngredients)
butterChickenIngredients = do 
  conn <- liftEffect connection
  query conn $ selectFrom recipeIngredients pure

main :: Effect Unit
main = launchAff_ do
  loadEnv
  liftEffect $ do 
    opts <- serverOptions
    mode <- lookupEnv "MODE"
    let 
      startupSuffix = maybe "" (\m -> i " in "m" mode") mode 
      startupMsg = i "starting server: "opts.hostname":"(show opts.port)"/"startupSuffix
      dist = if mode == Just "development" then "./dist" else "./release/dist"
    void $ HTTPure.serve' opts (logMiddleware (router dist)) $ log startupMsg 
  
