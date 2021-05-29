module Recipes.Frontend.Main where

import Frontend.Prelude

import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Data.Argonaut (printJsonDecodeError)
import Data.HTTP.Method (Method(..))
import Data.List (List(..))
import Data.List as List
import Recipes.API (RecipesValue, currentStateRoute, ingredientsRoute, recipesRoute, recipesWithStepsRoute, routeStr, selectRecipeRoute, setUseCaseRoute, submitRecipesRoute)
import Recipes.DataStructures (AppState, CurrentUseCase(..), Ingredient, ShoppingState(..))
import Recipes.Frontend.GroceryList (groceryList)
import Recipes.Frontend.Http (expectRequest)
import Recipes.Frontend.PantryList (pantryList)
import Recipes.Frontend.RecipeList (recipeList)
import Recipes.Frontend.RecipeSelection (recipeSelection)
import Recipes.Frontend.RecipeStepList (recipeStepList)
import Recipes.StateSerialization (decodeAppState)
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

loadRecipesWithSteps :: Aff RecipesValue 
loadRecipesWithSteps = do
  resp <- request $ defaultRequest { url = routeStr recipesWithStepsRoute, responseFormat = ResponseFormat.json }
  {body} <- resp # liftErrorVia printError 
  decodeJson body # liftErrorVia printJsonDecodeError 

selectRecipe :: String -> Aff Unit
selectRecipe recipe = 
  expectRequest $ defaultRequest 
    { method = Left POST, url = routeStr selectRecipeRoute
    , content = Just $ RequestBody.String recipe
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
  let recipeListItems = recipes <#> \name -> {name, checked: false}
  selected <- recipeList recipeListItems Nil
  liftAff $ submitRecipes selected
  liftEffect (window >>= location >>= reload)

useCaseBar :: Widget HTML Unit
useCaseBar = do
  useCase <- div [Props.style { width: "100%", padding: "0.5em", backgroundColor: "black" }]
    [ button [Props.onClick $> Shopping, Props.className "nav-button"] [text "Buy Some Food"]
    , button [Props.onClick $> Cooking, Props.className "nav-button"] [text "Cook Something"]
    ]

  liftAff $ expectRequest $ defaultRequest
    { url = routeStr setUseCaseRoute, method = Left POST
    , content = Just $ Json $ encodeJson useCase
    }

  liftEffect (window >>= location >>= reload)

content :: Widget HTML Unit
content = do
  appState <- (text "Loading..." <|> liftAff loadState)
  ( useCaseBar 
    <|>
    div [Props.style { marginLeft: "1em" }] 
      [ case appState of 
        {useCase: Shopping, shoppingState: InputRecipes} -> inputRecipes
        {useCase: Shopping, shoppingState: CheckKitchen ingredients} -> pantryList (ingredients <#> {checked: false, isCustom: false, item: _})
        {useCase: Shopping, shoppingState: BuyGroceries ingredients custom} -> 
          groceryList 
            (  (ingredients <#> {checked: false, isCustom: false, item: _})
            <> (custom <#> {checked: false, isCustom: true, item: _})
            )

        {useCase: Cooking, cookingState: Nothing} -> do
          recipes <- (text "Loading..." <|> liftAff loadRecipesWithSteps)
          selectedRecipe <- recipeSelection $ List.fromFoldable recipes
          liftAff $ selectRecipe selectedRecipe
          liftEffect (window >>= location >>= reload)

        {useCase: Cooking, cookingState: Just cookingState} -> recipeStepList cookingState
      ]
    <|>
    div [Props.style { height: "5em" }] []
  )

main :: Effect Unit
main = runWidgetInDom "contents" content
