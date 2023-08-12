module Recipes.Backend.LoadState where

import Backend.Prelude

import Control.Alternative (guard)
import Control.Monad.Except (runExceptT)
import Data.Array as Array
import Data.Codec.Argonaut as Codec
import Data.List (List)
import Data.List as List
import Data.Set as Set
import Recipes.API (RecipesValue)
import Recipes.Backend.CosmosDB (printDeleteError, printQueryError)
import Recipes.Backend.CosmosDB as Cosmos
import Recipes.Backend.DB (readAllIngredients, readAllRecipeIngredients, readAllRecipes, readAppState, recipeStepsCodec, recipeStepsContainer)
import Recipes.Backend.DB as DB
import Recipes.DataStructures (AppState, CookingState, RecipeIngredients, RecipeSteps, Ingredient)


allRecipes :: ExceptT String Aff RecipesValue
allRecipes = do 
  recipeNames <- readAllRecipes # withExceptT printQueryError
  pure (recipeNames <#> _.name)

allIngredients :: ExceptT String Aff $ List Ingredient
allIngredients = do
  readAllIngredients # withExceptT printQueryError <#> List.fromFoldable

allRecipeIngredients :: ExceptT String Aff $ List RecipeIngredients 
allRecipeIngredients = do
  readAllRecipeIngredients # withExceptT printQueryError <#> List.fromFoldable

getState :: ExceptT String Aff AppState
getState = do
  ingredients <- readAllIngredients # withExceptT printQueryError
  appStateRecord <- readAppState (List.fromFoldable ingredients) # withExceptT printQueryError

  appStateRecord # note "No appState record found in the database" # except

setState :: AppState -> ExceptT String Aff Unit
setState state = do
  ingredients <- readAllIngredients # withExceptT printQueryError

  DB.deleteAppState # withExceptT (printDeleteError "appState")
  DB.insertAppState (List.fromFoldable ingredients) state


addIngredient :: Ingredient -> ExceptT String Aff Unit
addIngredient = DB.insertIngredient

insertRecipeIfNotExists :: {name :: String} -> ExceptT String Aff Unit
insertRecipeIfNotExists recipe@{name} = do
  existingRecipes <- allRecipes
  when (not Array.elem name existingRecipes) $ DB.insertRecipe recipe

addShopRecipe :: {name :: String} -> List RecipeIngredients -> ExceptT String Aff Unit
addShopRecipe recipe ingredients = do
  insertRecipeIfNotExists recipe
  traverse_ DB.insertRecipeIngredients ingredients

upsertRecipeStep :: RecipeSteps -> ExceptT String Aff Unit
upsertRecipeStep step@{recipeName} = do
  insertRecipeIfNotExists {name: recipeName}
  DB.deleteRecipeSteps step # runExceptT # void # lift
  DB.insertRecipeSteps step

getSteps :: String -> ExceptT String Aff CookingState
getSteps recipeName = do
  stepsCol <- recipeStepsContainer

  steps <- 
    Cosmos.query recipeStepsCodec stepsCol 
      "SELECT * FROM recipeSteps WHERE recipeSteps.recipeName = @recipeName ORDER BY recipeSteps.stepNumber ASC" 
      [{ name: "@recipeName", value: encode Codec.string recipeName }]
    # withExceptT printQueryError

  guard (not $ Array.null steps) # note (i"No recipe steps associated with the recipe '"recipeName"'" :: String) # except

  let 
    cookingStateSteps = steps <#> \(step :: RecipeSteps) ->
      { completed: false, ordinal: step.stepNumber, description: step.stepDescription }

  pure { recipe: recipeName, steps: List.fromFoldable cookingStateSteps }

getRecipesWithSteps :: ExceptT String Aff $ Array String 
getRecipesWithSteps = do
  recipes <- DB.readAllRecipeSteps # withExceptT printQueryError
  pure $ Array.fromFoldable (Set.fromFoldable (recipes <#> _.recipeName))
