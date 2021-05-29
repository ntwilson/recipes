module Recipes.API where

import Shared.Prelude

import Recipes.DataStructures (CurrentUseCase, Ingredient, RecipeStep, SerializedAppState, StoreItem)

recipesRoute :: Array String
recipesRoute = ["api", "recipes"]
type RecipesValue = Array String

ingredientsRoute :: Array String
ingredientsRoute = ["api", "ingredients"]
type IngredientsValue = Array Ingredient

submitRecipesRoute :: Array String
submitRecipesRoute = ["api", "submit-recipes"]
type SubmitRecipesValue = Array String

submitPantryRoute :: Array String
submitPantryRoute = ["api", "submit-pantry"]

setItemStatusRoute :: Array String
setItemStatusRoute = ["api", "set-item-status"]
type SetItemStatusValue = { checked :: Boolean, item :: StoreItem, isCustom :: Boolean }

addItemRoute :: Array String
addItemRoute = ["api", "add-item"]
type AddItemValue = Ingredient

currentStateRoute :: Array String
currentStateRoute = ["api", "current-state"]
type CurrentStateValue = SerializedAppState

resetStateRoute :: Array String
resetStateRoute = ["api", "reset-state"]

resetRecipeRoute :: Array String
resetRecipeRoute = ["api", "reset-recipe"]

recipesWithStepsRoute :: Array String
recipesWithStepsRoute = ["api", "recipes-with-steps"]

selectRecipeRoute :: Array String
selectRecipeRoute = ["api", "select-recipe"]
type SelectRecipeValue = String

setRecipeStepStatusRoute :: Array String
setRecipeStepStatusRoute = ["api", "set-recipe-status"]
type SetRecipeStepStatusValue = RecipeStep

setUseCaseRoute :: Array String
setUseCaseRoute = ["api", "set-use-case"]
type SetUseCaseValue = CurrentUseCase

recipeStepsRoute :: Array String
recipeStepsRoute = ["api", "recipe-steps"]
type RecipeStepsValue = Array RecipeStep

routeStr :: Array String -> String
routeStr segments = i "/"(intercalate "/" segments)

