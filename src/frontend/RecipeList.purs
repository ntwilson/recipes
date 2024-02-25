module Recipes.Frontend.RecipeList where

import Frontend.Prelude

import Concur.React.Props as Props
import Data.List (List, (:))
import Data.List as List
import Recipes.Frontend.MUI as MUI

type RecipeListItem = {name :: String, checked :: Boolean}
nextRecipe :: ∀ f. Traversable f => f RecipeListItem -> Widget HTML RecipeListItem
nextRecipe allRecipes = 
  fold 
    (allRecipes <#> \recipe -> 
      div'
        [ MUI.checkbox $ MUI.checkboxProps { onClick: \(_::Boolean) -> recipe, checked: recipe.checked, inputProps: { "aria-label": "controlled" } }
        , span [Props.onClick $> recipe, Props.className "checkbox-text"] [text recipe.name]
        ]
    )

data RecipeSelection = AnotherRecipe String | SubmitRecipes
recipeList :: ∀ f. Traversable f => f RecipeListItem -> List String -> Widget HTML $ List String
recipeList allRecipes selectedRecipesSoFar = do
  selection <- 
    fold
      [ h3' [text "What would you like to eat this week?"] 
      , nextRecipe allRecipes <#> (AnotherRecipe <<< _.name)
      , br'
      , div' [button [Props.onClick] [text "Submit"]] $> SubmitRecipes
      ]

  case selection of 
    SubmitRecipes -> pure selectedRecipesSoFar
    AnotherRecipe recipe -> recipeList (toggle recipe allRecipes) (updateSelection recipe)

  where 
    updateSelection nextSelected = 
      if nextSelected `elem` selectedRecipesSoFar
      then List.delete nextSelected selectedRecipesSoFar
      else nextSelected : selectedRecipesSoFar

    toggle recipeName recipes =
      recipes <#> \recipe -> 
        if recipe.name == recipeName then recipe { checked = not recipe.checked } else recipe
