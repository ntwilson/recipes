module Recipes.Frontend.Main where

import Frontend.Prelude

import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Data.Argonaut (printJsonDecodeError)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.List as List
import Recipes.API (RecipesValue, currentStateRoute, ingredientsRoute, recipesRoute, routeStr, submitRecipesRoute)
import Recipes.DataStructures (AppState(..), Ingredient, StoreItem, decodeAppState)
import Recipes.ErrorHandling (throw)
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

recipeList :: ∀ f. Traversable f => f String -> Widget HTML String
recipeList allRecipes = 
  fold (allRecipes <#> \recipe -> div' [input [Props._type "checkbox", Props.onChange $> recipe] <|> text recipe])

data RecipeSelection = AnotherSelection String | Submit
selectedRecipes :: ∀ f. Traversable f => f String -> List String -> Widget HTML $ List String
selectedRecipes allRecipes selectedRecipesSoFar = do
  selection <- 
    fold
      [ recipeList allRecipes <#> AnotherSelection
      , div' [button [Props.onClick] [text "Submit"]] $> Submit
      ]

  case selection of 
    Submit -> pure selectedRecipesSoFar
    AnotherSelection recipe -> selectedRecipes allRecipes $ updateSelection recipe

  where 
    updateSelection nextSelected = 
      if nextSelected `elem` selectedRecipesSoFar
      then List.delete nextSelected selectedRecipesSoFar
      else nextSelected : selectedRecipesSoFar

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
  selected <- selectedRecipes recipes Nil
  liftAff $ submitRecipes selected
  liftEffect (window >>= location >>= reload)

groceryList :: List StoreItem -> Widget HTML Unit
groceryList items = do 
  -- , Props.onChange $> name
  fold (items <#> \{amount, ingredient:{name}} -> div' [input [Props._type "checkbox"] <|> text (i amount" "name)])

content :: Widget HTML Unit
content = do
  appState <- (text "Loading..." <|> liftAff loadState)
  case appState of 
    InputRecipes -> inputRecipes
    CheckKitchen ingredients -> groceryList ingredients
    BuyGroceries ingredients -> groceryList ingredients



-- content :: Widget HTML Void
-- content = do
--   recipes <- (text "Loading..." <|> liftAff loadRecipes)
--   selected <- selectedRecipes recipes Nil
--   liftAff $ submitRecipes selected
--   text "Recipes submitted."

main :: Effect Unit
main = runWidgetInDom "contents" content
