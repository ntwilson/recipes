module Recipes.API where

import Shared.Prelude

import Recipes.DataStructures (SerializedAppState, Ingredient)

recipesRoute :: Array String
recipesRoute = ["api", "recipes"]
type RecipesValue = Array String

ingredientsRoute :: Array String
ingredientsRoute = ["api", "ingredients"]
type IngredientsValue = Array Ingredient

submitRecipesRoute :: Array String
submitRecipesRoute = ["api", "submitRecipes"]
type SubmitRecipesValue = Array String

currentStateRoute :: Array String
currentStateRoute = ["api", "currentState"]
type CurrentStateValue = SerializedAppState

resetStateRoute :: Array String
resetStateRoute = ["api", "resetState"]

routeStr :: Array String -> String
routeStr segments = i "/"(intercalate "/" segments)

