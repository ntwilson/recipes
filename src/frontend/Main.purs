module Recipes.Frontend.Main where

import Frontend.Prelude

import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Run (runWidgetInDom)
import Data.Argonaut (printJsonDecodeError)
import Data.HTTP.Method (Method(..))
import Data.List (List(..))
import Recipes.API (RecipesValue, currentStateRoute, ingredientsRoute, recipesRoute, routeStr, submitRecipesRoute)
import Recipes.StateSerialization (decodeAppState)
import Recipes.DataStructures (AppState, CurrentUseCase(..), Ingredient, ShoppingState(..))
import Recipes.Frontend.GroceryList (groceryList)
import Recipes.Frontend.Http (expectRequest)
import Recipes.Frontend.PantryList (pantryList)
import Recipes.Frontend.RecipeList (recipeList)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

loadRecipes :: Aff RecipesValue
loadRecipes = do
  resp <- request $ defaultRequest { url = routeStr recipesRoute, responseFormat = ResponseFormat.json }
  {body} <- resp # liftErrorVia printError 
  decodeJson body # liftErrorVia printJsonDecodeError 

submitRecipes :: List String -> Aff Unit
submitRecipes recipes = 
  expectRequest $ defaultRequest 
    { method = Left POST, url = routeStr submitRecipesRoute
    , content = Just $ RequestBody.Json $ encodeJson recipes
    }

loadIngredients :: Aff $ List Ingredient
loadIngredients = do
  resp <- request $ defaultRequest { url = routeStr ingredientsRoute, responseFormat = ResponseFormat.json }
  {body} <- resp # liftErrorVia printError 
  decodeJson body # liftErrorVia printJsonDecodeError 

loadState :: Aff AppState 
loadState = do
  resp <- request $ defaultRequest { url = routeStr currentStateRoute, responseFormat = ResponseFormat.json }
  {body} <- resp # liftErrorVia printError 
  serialized <- decodeJson body # liftErrorVia printJsonDecodeError 
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
    {useCase: Shopping, shoppingState: InputRecipes} -> inputRecipes
    {useCase: Shopping, shoppingState: CheckKitchen ingredients} -> pantryList (ingredients <#> {checked: false, isCustom: false, item: _})
    {useCase: Shopping, shoppingState: BuyGroceries ingredients custom} -> 
      groceryList 
        (  (ingredients <#> {checked: false, isCustom: false, item: _})
        <> (custom <#> {checked: false, isCustom: true, item: _})
        )
    _ -> liftEffect $ throw "Unsupported app state.  Try resetting the state and trying this state later"

main :: Effect Unit
main = runWidgetInDom "contents" content
