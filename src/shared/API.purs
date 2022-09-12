module Recipes.API where

import Shared.Prelude hiding ((/))

import Data.Generic.Rep (class Generic, NoArguments)
import Recipes.DataStructures (CurrentUseCase, Ingredient, RecipeStep, StoreItem, AppState)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as Routing
import Routing.Duplex.Generic as Routing
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)


data RecipeRoute 
  = Home
  | CSS
  | JS
  | Recipes 
  | RecipesWithSteps
  | Ingredients
  | SubmitRecipes
  | SubmitPantry
  | SetItemStatus
  | AddItem
  | CurrentState
  | ResetState
  | ResetRecipe
  | SelectRecipe
  | SetRecipeStatus
  | SetUseCase
  | RecipeSteps
  

derive instance Eq RecipeRoute
derive instance Generic RecipeRoute _

recipeRouteDuplex :: RouteDuplex' RecipeRoute
recipeRouteDuplex = 
  Routing.root $ Routing.sum 
    { "Home": Routing.noArgs
    , "CSS": rootPath "index.css"
    , "JS": rootPath "main.js"
    , "Recipes": apiPath "recipes" 
    , "RecipesWithSteps": apiPath "recipes-with-steps"
    , "Ingredients": apiPath "ingredients"
    , "SubmitRecipes": apiPath "submit-recipes"
    , "SubmitPantry": apiPath "submit-pantry"
    , "SetItemStatus": apiPath "set-item-status"
    , "AddItem": apiPath "add-item"
    , "CurrentState": apiPath "current-state"
    , "ResetState": apiPath "reset-state"
    , "ResetRecipe": apiPath "reset-recipe"
    , "SelectRecipe": apiPath "select-recipe"
    , "SetRecipeStatus": apiPath "set-recipe-status"
    , "SetUseCase": apiPath "set-use-case"
    , "RecipeSteps": apiPath "recipe-steps"
    }

  where
  rootPath route = Routing.path route Routing.noArgs
  apiPath :: String -> RouteDuplex' NoArguments
  apiPath route = "api" / Routing.path route Routing.noArgs

type RecipesValue = Array String
type IngredientsValue = Array Ingredient
type SubmitRecipesValue = Array String
type SetItemStatusValue = { checked :: Boolean, item :: StoreItem, isCustom :: Boolean }
type AddItemValue = Ingredient
type CurrentStateValue = AppState
type SelectRecipeValue = String
type SetRecipeStepStatusValue = RecipeStep
type SetUseCaseValue = CurrentUseCase
type RecipeStepsValue = Array RecipeStep

routeStr :: Array String -> String
routeStr segments = i "/"(intercalate "/" segments)

parse :: String -> Either RouteError RecipeRoute
parse = Routing.parse recipeRouteDuplex

print :: RecipeRoute -> String
print = Routing.print recipeRouteDuplex
