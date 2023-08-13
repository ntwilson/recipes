module Recipes.Frontend.Main where

import Frontend.Prelude

import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Data.Codec.Argonaut.Common as Codec
import Data.HTTP.Method (Method(..))
import Data.List (List(..))
import Data.List as List
import Recipes.API (RecipesValue)
import Recipes.API as Routing
import Recipes.DataStructures (AppState, CurrentUseCase(..), Ingredient, ShoppingState(..), appStateCodec, ingredientCodec, useCaseCodec)
import Recipes.Frontend.GroceryList (groceryList)
import Recipes.Frontend.Http (expectRequest, expectRequest', runRequest)
import Recipes.Frontend.PantryList (pantryList)
import Recipes.Frontend.RecipeList (recipeList)
import Recipes.Frontend.RecipeSelection (recipeSelection)
import Recipes.Frontend.RecipeStepList (recipeStepList)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

loadRecipes :: ∀ r. Run (AFFECT + EXCEPT String + r) RecipesValue
loadRecipes = do
  {body} <- runRequest $ defaultRequest { url = Routing.print Routing.Recipes, responseFormat = ResponseFormat.json }
  decode (Codec.array Codec.string) body # lmap printJsonDecodeError # rethrow

submitRecipes :: ∀ r. List String -> Run (AFFECT + EXCEPT String + r) Unit
submitRecipes recipes = 
  expectRequest' $ defaultRequest 
    { method = Left POST, url = Routing.print Routing.SubmitRecipes
    , content = Just $ RequestBody.Json $ encode (Codec.list Codec.string) recipes
    }

loadRecipesWithSteps :: ∀ r. Run (AFFECT + EXCEPT String + r) RecipesValue 
loadRecipesWithSteps = do
  {body} <- runRequest $ defaultRequest { url = Routing.print Routing.RecipesWithSteps, responseFormat = ResponseFormat.json }
  decode (Codec.array Codec.string) body # lmap printJsonDecodeError # rethrow

selectRecipe :: ∀ r. String -> Run (AFFECT + EXCEPT String + r) Unit
selectRecipe recipe = 
  expectRequest' $ defaultRequest 
    { method = Left POST, url = Routing.print Routing.SelectRecipe
    , content = Just $ RequestBody.String recipe
    }

loadIngredients :: ∀ r. Run (AFFECT + EXCEPT String + r) $ List Ingredient
loadIngredients = do
  {body} <- runRequest $ defaultRequest { url = Routing.print Routing.Ingredients, responseFormat = ResponseFormat.json }
  decode (Codec.list ingredientCodec) body # lmap printJsonDecodeError # rethrow

loadState :: ∀ r. Run (AFFECT + EXCEPT String + r) AppState 
loadState = do
  {body} <- runRequest $ defaultRequest { url = Routing.print Routing.CurrentState, responseFormat = ResponseFormat.json }
  ingredients <- loadIngredients
  Codec.decode (appStateCodec ingredients) body # lmap Codec.printJsonDecodeError # rethrow

inputRecipes :: Widget HTML Unit 
inputRecipes = do 
  recipes <- (text "Loading..." <|> runToWidget loadRecipes)
  let recipeListItems = recipes <#> \name -> {name, checked: false}
  selected <- recipeList recipeListItems Nil
  runToWidget $ submitRecipes selected
  liftEffect (window >>= location >>= reload)

useCaseBar :: CurrentUseCase -> Widget HTML Unit
useCaseBar currentUseCase = do
  useCase <- div [Props.className "nav-bar" ]
    [ span (if currentUseCase == Shopping then [Props.className "highlighted"] else [])
      [ button [Props.onClick $> Shopping, Props.className "nav-button"] [text "SHOP"] ]
    , span (if currentUseCase == Cooking then [Props.className "highlighted"] else [])
      [ button [Props.onClick $> Cooking, Props.className "nav-button"] [text "COOK"] ]
    ]

  liftAff $ expectRequest $ defaultRequest
    { url = Routing.print Routing.SetUseCase, method = Left POST
    , content = Just $ Json $ encode useCaseCodec useCase
    }

  liftEffect (window >>= location >>= reload)

content :: Widget HTML Unit
content = do
  appState <- (text "Loading..." <|> runToWidget loadState)
  ( useCaseBar appState.useCase 
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
          recipes <- (text "Loading..." <|> runToWidget loadRecipesWithSteps)
          selectedRecipe <- recipeSelection $ List.fromFoldable recipes
          runToWidget $ selectRecipe selectedRecipe
          liftEffect (window >>= location >>= reload)

        {useCase: Cooking, cookingState: Just cookingState} -> recipeStepList cookingState
      ]
    <|>
    div [Props.style { height: "5em" }] []
  )

main :: Effect Unit
main = runWidgetInDom "contents" content
