module Recipes.Backend.Main where

import Backend.Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.List (List, (:))
import Data.List as List
import HTTPure as HTTPure
import HTTPure.Method (Method(..))
import Node.Encoding (Encoding(..))
import Recipes.API (RecipesValue, SetItemStatusValue, currentStateRoute, ingredientsRoute, recipesRoute, resetStateRoute, setItemStatusRoute, submitRecipesRoute)
import Recipes.Backend.DB (appState, connection, execQuery, execUpdate, ingredient, recipe, recipeIngredients)
import Recipes.Backend.ServerSetup (loadEnv, logMiddleware, serverOptions)
import Recipes.DataStructures (AppState(..), Ingredient, RecipeIngredients, SerializedAppState, decodeAppState, encodeAppState)
import Recipes.RecipesToIngredients (recipesToIngredients)
import Selda (selectFrom)
import Selda as Selda

router :: String -> HTTPure.Request -> HTTPure.ResponseM
router dist rqst = 
  catchError (rtr rqst.method rqst.path) errHandler
  where
    rtr Get [] = do
      contents <- readTextFile UTF8 (i dist"/index.html")
      HTTPure.ok' (HTTPure.header "Content-Type" "text/html; charset=UTF-7") contents

    rtr Get ["main.js"] = do
      contents <- readTextFile UTF8 (i dist"/main.js")
      HTTPure.ok' (HTTPure.header "Content-Type" "text/javascript") contents

    rtr Get route | route == recipesRoute = do
      contents <- encodeJson <$> allRecipes 
      HTTPure.ok $ Json.stringify contents

    rtr Get route | route == ingredientsRoute = do
      contents <- encodeJson <$> allIngredients
      HTTPure.ok $ Json.stringify contents

    rtr Post route | route == submitRecipesRoute = go 
      where 
        go
          | Right json <- Json.parseJson rqst.body
          , Right (submittedRecipes :: List String) <- decodeJson json = do
            pairings <- allRecipeIngredients
            ingredients <- allIngredients
            let groceryList = recipesToIngredients pairings ingredients submittedRecipes
            setState (CheckKitchen groceryList)
            HTTPure.noContent
          
          | otherwise = HTTPure.badRequest "Could not parse request body"

    rtr Get route | route == currentStateRoute = do
      state <- getSerializedState
      HTTPure.ok $ Json.stringify $ encodeJson state

    rtr Get route | route == resetStateRoute = do
      setState InputRecipes
      HTTPure.noContent

    rtr Post route | route == setItemStatusRoute = go
      where
        go 
          | Right json <- Json.parseJson rqst.body
          , Right (submittedItem :: SetItemStatusValue) <- decodeJson json = do
            state <- getState 
            case state of 
              InputRecipes -> HTTPure.conflict "No items can or will exist until recipes are input."
              CheckKitchen items -> do
                setState $ CheckKitchen $ processItem submittedItem items
                HTTPure.noContent 
              BuyGroceries items -> do
                setState $ CheckKitchen $ processItem submittedItem items
                HTTPure.noContent

          | otherwise = HTTPure.badRequest "Could not parse request body"
        
        processItem { checked, item: submittedItem } items 
          | checked = List.filter (_.ingredient.name >>> (/=) submittedItem.ingredient.name) items
          | otherwise = submittedItem : items


    rtr _ _ = HTTPure.notFound
    
    errHandler err = HTTPure.internalServerError $ show err
  
allRecipes :: Aff RecipesValue
allRecipes = do
  conn <- connection
  ( execQuery conn $ selectFrom recipe (\{name} -> pure {name})) <##> _.name

allIngredients :: Aff $ List Ingredient
allIngredients = do
  conn <- connection 
  List.fromFoldable <$> (execQuery conn $ selectFrom ingredient pure)

allRecipeIngredients :: Aff $ List RecipeIngredients 
allRecipeIngredients = do
  conn <- connection
  List.fromFoldable <$> (execQuery conn $ selectFrom recipeIngredients pure)

getSerializedState :: Aff SerializedAppState
getSerializedState = do
  conn <- connection
  serializedRecords <- execQuery conn $ selectFrom appState pure 
  Array.head serializedRecords # note "No appState record found in the database" # liftError

getState :: Aff AppState
getState = do
  conn <- connection
  ingredients <- execQuery conn $ selectFrom ingredient pure
  serializedRecords <- execQuery conn $ selectFrom appState pure 
  serialized <- Array.head serializedRecords # note "No appState record found in the database" # liftError
  decodeAppState (List.fromFoldable ingredients) serialized

setState :: AppState -> Aff Unit
setState state = do
  conn <- connection
  let stateRecord = encodeAppState state
  execUpdate conn appState (const $ Selda.lit true)  
    (const {name: Selda.lit stateRecord.name, ingredients: Selda.lit stateRecord.ingredients}) 


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
  
