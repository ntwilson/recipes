module Recipes.Frontend.RecipeList where

import Frontend.Prelude

import Concur.React.Props as Props
import Data.List (List, (:))
import Data.List as List


nextRecipe :: ∀ f. Traversable f => f String -> Widget HTML String
nextRecipe allRecipes = 
  fold (allRecipes <#> \recipe -> div' [input [Props._type "checkbox", Props.onChange $> recipe] <|> text recipe])

data RecipeSelection = AnotherRecipe String | SubmitRecipes
recipeList :: ∀ f. Traversable f => f String -> List String -> Widget HTML $ List String
recipeList allRecipes selectedRecipesSoFar = do
  selection <- 
    fold
      [ h1' [text "What would you like to eat this week?"] 
      , nextRecipe allRecipes <#> AnotherRecipe
      , br'
      , div' [button [Props.onClick] [text "Submit"]] $> SubmitRecipes
      ]

  case selection of 
    SubmitRecipes -> pure selectedRecipesSoFar
    AnotherRecipe recipe -> recipeList allRecipes $ updateSelection recipe

  where 
    updateSelection nextSelected = 
      if nextSelected `elem` selectedRecipesSoFar
      then List.delete nextSelected selectedRecipesSoFar
      else nextSelected : selectedRecipesSoFar
