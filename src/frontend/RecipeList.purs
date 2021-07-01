module Recipes.Frontend.RecipeList where

import Frontend.Prelude

import Concur.React.Props as Props
import Data.List (List, (:))
import Data.List as List

type RecipeListItem = {name :: String, checked :: Boolean}
nextRecipe :: ∀ f. Traversable f => f RecipeListItem -> Widget HTML RecipeListItem
nextRecipe allRecipes = 
  fold (allRecipes <#> \recipe -> 
    div [Props.className "checkbox-container"]
      [ input [Props._type "checkbox", Props.onChange $> recipe, Props.checked recipe.checked] 
      , span [Props.onClick $> recipe, Props.className "checkbox-text"] [text recipe.name]
      ]
  )

data RecipeSelection = AnotherRecipe String | SubmitRecipes
recipeList :: ∀ f. Traversable f => f RecipeListItem -> List String -> Widget HTML $ List String
recipeList allRecipes selectedRecipesSoFar = do
  selection <- 
    fold
      [ h3' [text "What would you like to eat this week?"] 
      , nextRecipe allRecipes <#> (_.name >>> AnotherRecipe)
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
