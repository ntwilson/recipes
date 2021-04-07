module Recipes.Frontend.Main where

import Frontend.Prelude

import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Run (runWidgetInDom)
import Data.Argonaut (printJsonDecodeError)
import Data.HTTP.Method (Method(..))
import Data.List (List(..))
import Recipes.API (RecipesValue, currentStateRoute, ingredientsRoute, recipesRoute, routeStr, submitRecipesRoute)
import Recipes.DataStructures (AppState(..), Ingredient, decodeAppState)
import Recipes.Frontend.GroceryList (groceryList)
import Recipes.Frontend.PantryList (pantryList)
import Recipes.Frontend.RecipeList (recipeList)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

loadRecipes :: Aff RecipesValue
loadRecipes = do
  resp <- request $ defaultRequest { url = routeStr recipesRoute, responseFormat = ResponseFormat.json }
  {body} <- resp # lmap printError # liftError
  decodeJson body # lmap printJsonDecodeError # liftError

submitRecipes :: List String -> Aff Unit
submitRecipes recipes = do
  tryResp <- request $ defaultRequest 
    { method = Left POST, url = routeStr submitRecipesRoute, responseFormat = ResponseFormat.string 
    , content = Just $ RequestBody.Json $ encodeJson recipes
    }

  resp <- tryResp # lmap printError # liftError
  if between 200 299 $ unwrap resp.status 
  then pure unit
  else throw (i"status "(show $ unwrap resp.status)". "(resp.body) :: String)

loadIngredients :: Aff $ List Ingredient
loadIngredients = do
  resp <- request $ defaultRequest { url = routeStr ingredientsRoute, responseFormat = ResponseFormat.json }
  {body} <- resp # lmap printError # liftError
  decodeJson body # lmap printJsonDecodeError # liftError

loadState :: Aff AppState 
loadState = do
  resp <- request $ defaultRequest { url = routeStr currentStateRoute, responseFormat = ResponseFormat.json }
  {body} <- resp # lmap printError # liftError
  serialized <- decodeJson body # lmap printJsonDecodeError # liftError
  ingredients <- loadIngredients
  decodeAppState ingredients serialized

inputRecipes :: Widget HTML Unit 
inputRecipes = do 
  recipes <- (text "Loading..." <|> liftAff loadRecipes)
  selected <- recipeList recipes Nil
  liftAff $ submitRecipes selected
  liftEffect (window >>= location >>= reload)

content :: Widget HTML Unit
content = do
  appState <- (text "Loading..." <|> liftAff loadState)
  case appState of 
    InputRecipes -> inputRecipes
    CheckKitchen ingredients -> pantryList (ingredients <#> {checked: false, item: _})
    BuyGroceries ingredients -> groceryList (ingredients <#> {checked: false, item: _})

main :: Effect Unit
main = runWidgetInDom "contents" content
