module Recipes.Backend.LoadState where

import Backend.Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Codec.Argonaut as Codec
import Data.List (List)
import Data.List as List
import Data.Set as Set
import Recipes.API (RecipesValue)
import Recipes.Backend.DB (appStateContainer, ingredientsContainer, newConnection, printQueryError, readAll, readAllWith, recipeContainer, recipeIngredientsContainer, recipeStepsContainer)
import Recipes.Backend.DB as DB
import Recipes.DataStructures (AppState, CookingState, Ingredient, RecipeIngredients, SerializedAppState, appStateCodec)
import Recipes.StateSerialization (encodeAppState)
import Unsafe.Coerce (unsafeCoerce)


allRecipes :: ExceptT String Aff RecipesValue
allRecipes = do 
  conn <- newConnection
  container <- recipeContainer conn
  recipeNames <- readAll container # withExceptT printQueryError
  pure (recipeNames <#> _.name)

allIngredients :: ExceptT String Aff $ List Ingredient
allIngredients = do
  conn <- newConnection
  container <- ingredientsContainer conn
  readAll container # withExceptT printQueryError <#> List.fromFoldable

allRecipeIngredients :: ExceptT String Aff $ List RecipeIngredients 
allRecipeIngredients = do
  conn <- newConnection
  container <- recipeIngredientsContainer conn
  readAll container # withExceptT printQueryError <#> List.fromFoldable

getSerializedState :: ExceptT String Aff SerializedAppState
getSerializedState = encodeAppState <$> getState

getState :: ExceptT String Aff AppState
getState = do
  conn <- newConnection
  ingredientCol <- ingredientsContainer conn
  appStateCol <- appStateContainer conn

  ingredients <- readAll ingredientCol # withExceptT printQueryError
  appStateRecords <- readAllWith (appStateCodec $ List.fromFoldable ingredients) appStateCol # withExceptT printQueryError

  Array.head appStateRecords # note "No appState record found in the database" # except

setState :: AppState -> ExceptT String Aff Unit
setState state = do
  conn <- newConnection
  appStateCol <- appStateContainer conn

  appStateRecords <- readAllWith (unsafeCoerce Codec.json) appStateCol # withExceptT printQueryError

  oldState <- Array.head appStateRecords # note "No appState record found in the database" # except
  DB.deleteWith (unsafeCoerce Codec.json) appStateCol oldState
  DB.insert appStateCol state



getSteps :: String -> ExceptT String Aff CookingState
getSteps recipeName = do
  conn <- newConnection
  stepsCol <- recipeStepsContainer conn

  steps <- DB.query stepsCol 
    "SELECT * FROM recipeSteps WHERE recipeName = @recipeName ORDER BY stepNumber ASC" 
    [{ name: "@recipeName", value: encodeJson recipeName }]
    # withExceptT printQueryError

  guard (not $ Array.null steps) # note (i"No recipe steps associated with the recipe '"recipeName"'" :: String) # except

  let 
    cookingStateSteps = steps <#> \step ->
      { completed: false, ordinal: step.stepNumber, description: step.stepDescription }

  pure { recipe: recipeName, steps: List.fromFoldable cookingStateSteps }

getRecipesWithSteps :: ExceptT String Aff $ Array String 
getRecipesWithSteps = do
  conn <- newConnection
  stepsCol <- recipeStepsContainer conn

  recipes <- DB.readAll stepsCol # withExceptT printQueryError

  pure $ Array.fromFoldable (Set.fromFoldable (recipes <#> _.recipeName))
