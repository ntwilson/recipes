module Recipes.API where

import Shared.Prelude

recipesRoute :: Array String
recipesRoute = ["api", "recipes"]
type RecipesValue = Array String

submitRecipesRoute :: Array String
submitRecipesRoute = ["api", "submitRecipes"]
type SubmitRecipesValue = Array String

routeStr :: Array String -> String
routeStr segments = i "/"(intercalate "/" segments)

