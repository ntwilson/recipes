module Recipes.Backend.LoadState where

import Backend.Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Codec.Argonaut as Codec
import Data.List (List)
import Data.List as List
import Data.Set as Set
import Recipes.API (RecipesValue)
import Recipes.Backend.CosmosDB (printDeleteError, printQueryError)
import Recipes.Backend.CosmosDB as Cosmos
import Recipes.Backend.DB (readAppState, readAllIngredients, readAllRecipeIngredients, readAllRecipes, recipeStepsCodec, recipeStepsContainer)
import Recipes.Backend.DB as DB
import Recipes.DataStructures (AppState, CookingState, Ingredient, RecipeIngredients, RecipeSteps)


allRecipes :: Run (AFFECT + EXCEPT String ()) RecipesValue
allRecipes = do 
  recipeNames <- readAllRecipes # printQueryError
  pure (recipeNames <#> _.name)

allIngredients :: Run (AFFECT + EXCEPT String ()) (List Ingredient)
allIngredients = do
  readAllIngredients # printQueryError <#> List.fromFoldable

allRecipeIngredients :: Run (AFFECT + EXCEPT String ()) (List RecipeIngredients) 
allRecipeIngredients = do
  readAllRecipeIngredients # printQueryError <#> List.fromFoldable

getState :: Run (AFFECT + EXCEPT String ()) AppState
getState = do
  ingredients <- readAllIngredients # printQueryError
  appStateRecord <- readAppState (List.fromFoldable ingredients) # printQueryError

  appStateRecord # note "No appState record found in the database" # rethrow

setState :: AppState -> Run (AFFECT + EXCEPT String ()) Unit
setState state = do
  ingredients <- readAllIngredients # printQueryError

  DB.deleteAppState # printDeleteError "appState"
  DB.insertAppState (List.fromFoldable ingredients) state



getSteps :: String -> Run (AFFECT + EXCEPT String ()) CookingState
getSteps recipeName = do
  stepsCol <- recipeStepsContainer

  steps <- 
    Cosmos.query recipeStepsCodec stepsCol 
      "SELECT * FROM recipeSteps WHERE recipeSteps.recipeName = @recipeName ORDER BY recipeSteps.stepNumber ASC" 
      [{ name: "@recipeName", value: encode Codec.string recipeName }]
    # printQueryError

  guard (not $ Array.null steps) # note (i"No recipe steps associated with the recipe '"recipeName"'" :: String) # rethrow

  let 
    cookingStateSteps = steps <#> \(step :: RecipeSteps) ->
      { completed: false, ordinal: step.stepNumber, description: step.stepDescription }

  pure { recipe: recipeName, steps: List.fromFoldable cookingStateSteps }

getRecipesWithSteps :: Run (AFFECT + EXCEPT String ()) (Array String) 
getRecipesWithSteps = do
  recipes <- DB.readAllRecipeSteps # printQueryError
  pure $ Array.fromFoldable (Set.fromFoldable (recipes <#> _.recipeName))

