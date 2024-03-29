module Recipes.Backend.LoadState where

import Backend.Prelude

import Control.Alternative (guard)
import Control.Monad.Except.Trans (except)
import Data.Array as Array
import Data.Codec.Argonaut as Codec
import Data.List (List)
import Data.List as List
import Data.Set as Set
import Recipes.API (RecipesValue)
import Recipes.Backend.CosmosDB (DELETE_ERROR, QUERY_ERROR)
import Recipes.Backend.CosmosDB as Cosmos
import Recipes.Backend.DB (readAppState, readAllIngredients, readAllRecipeIngredients, readAllRecipes, recipeStepsCodec, recipeStepsContainer)
import Recipes.Backend.DB as DB
import Recipes.DataStructures (AppState, CookingState, Ingredient, RecipeIngredients, RecipeSteps)


allRecipes :: ∀ r m. MonadAff m => ExceptV (QUERY_ERROR + r) m RecipesValue
allRecipes = do 
  recipeNames <- readAllRecipes
  pure (recipeNames <#> _.name)

allIngredients :: ∀ r m. MonadAff m => ExceptV (QUERY_ERROR + r) m (List Ingredient)
allIngredients = do
  readAllIngredients <#> List.fromFoldable

allRecipeIngredients :: ∀ r m. MonadAff m => ExceptV (QUERY_ERROR + r) m (List RecipeIngredients) 
allRecipeIngredients = do
  readAllRecipeIngredients <#> List.fromFoldable
 
getState :: ∀ r m. MonadAff m => ExceptV (QUERY_ERROR + STRING_ERROR + r) m AppState
getState = do
  ingredients <- readAllIngredients
  appStateRecord <- readAppState (List.fromFoldable ingredients)

  appStateRecord # note (stringError "No appState record found in the database") # except

setState :: ∀ r m. MonadAff m => AppState -> ExceptV (DELETE_ERROR + STRING_ERROR + r) m Unit
setState state = do
  ingredients <- readAllIngredients

  DB.deleteAppState
  DB.insertAppState (List.fromFoldable ingredients) state



getSteps :: ∀ r m. MonadAff m => String -> ExceptV (QUERY_ERROR + STRING_ERROR + r) m CookingState
getSteps recipeName = do
  stepsCol <- recipeStepsContainer

  steps <- 
    Cosmos.query recipeStepsCodec stepsCol 
      "SELECT * FROM recipeSteps WHERE recipeSteps.recipeName = @recipeName ORDER BY recipeSteps.stepNumber ASC" 
      [{ name: "@recipeName", value: encode Codec.string recipeName }]

  guard (not $ Array.null steps) # note (stringError $ i"No recipe steps associated with the recipe '"recipeName"'") # except

  let 
    cookingStateSteps = steps <#> \(step :: RecipeSteps) ->
      { completed: false, ordinal: step.stepNumber, description: step.stepDescription }

  pure { recipe: recipeName, steps: List.fromFoldable cookingStateSteps }

getRecipesWithSteps :: ∀ r m. MonadAff m => ExceptV (QUERY_ERROR + STRING_ERROR + r) m (Array String) 
getRecipesWithSteps = do
  recipes <- DB.readAllRecipeSteps
  pure $ Array.fromFoldable (Set.fromFoldable (recipes <#> _.recipeName))

