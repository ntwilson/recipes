module Recipes.Frontend.Main where

import Frontend.Prelude

import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Data.Argonaut (printJsonDecodeError)
import Data.List (List(..), (:))
import Data.List as List
import Recipes.API (RecipesValue, recipesRoute, routeStr)

loadRecipes :: Aff RecipesValue
loadRecipes = do
  resp <- request (defaultRequest { url = routeStr recipesRoute, responseFormat = ResponseFormat.json })
  {body} <- resp # lmap printError # liftError
  decodeJson body # lmap printJsonDecodeError # liftError

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

content :: Widget HTML Void
content = do
  recipes <- (text "Loading..." <|> liftAff loadRecipes)
  selected <- selectedRecipes recipes Nil
  text $ i "You selected: "(show selected)

main :: Effect Unit
main = runWidgetInDom "contents" content
