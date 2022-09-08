module Recipes.Backend.LoadState where

import Backend.Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Set as Set
import Recipes.API (RecipesValue)
import Recipes.Backend.DB (QueryError(..), newConnection, printQueryError, readAll, recipeContainer)
import Recipes.DataStructures (AppState, CookingState, Ingredient, RecipeIngredients, SerializedAppState, SerializedAppStateDB)
import Recipes.StateSerialization (decodeAppState, encodeAppState)


allRecipes :: ExceptT String Aff RecipesValue
allRecipes = do 
  conn <- newConnection
  container <- recipeContainer conn
  recipeNames <- readAll container # withExceptT printQueryError
  pure (recipeNames <#> _.name)

-- allIngredients :: Aff $ List Ingredient
-- allIngredients = withConnection $ \conn ->
--   List.fromFoldable <$> (execQuery conn $ Query $ i"SELECT * FROM "ingredientTable)

-- allRecipeIngredients :: Aff $ List RecipeIngredients 
-- allRecipeIngredients = withConnection $ \conn ->
--   List.fromFoldable <$> (execQuery conn $ Query $ i"SELECT * FROM "recipeIngredientsTable)

-- getSerializedState :: Aff SerializedAppState
-- getSerializedState = formatFromDB <$> state 
--   where 
--     state = withConnection $ \conn -> do
--       serializedRecords <- execQuery conn $ Query $ i"SELECT * FROM "appStateTable
--       Array.head serializedRecords # note "No appState record found in the database" # liftError

--     formatFromDB :: SerializedAppStateDB -> SerializedAppState
--     formatFromDB { name, ingredients, recipesteps } = { name, ingredients, recipeSteps: recipesteps }

-- getState :: Aff AppState
-- getState = withConnection $ \conn -> do
--   ingredients <- execQuery conn $ Query $ i"SELECT * FROM "ingredientTable
--   serializedRecords <- execQuery conn $ Query $ i"SELECT * FROM "appStateTable
--   serialized <- Array.head serializedRecords # note "No appState record found in the database" # liftError
--   decodeAppState (List.fromFoldable ingredients) serialized

-- setState :: AppState -> Aff Unit
-- setState state = withConnection $ \conn -> do
--   let stateRecord = encodeAppState state
--   execUpdate conn (Query $ i"UPDATE "appStateTable" SET name = $1, ingredients = $2, recipeSteps = $3")
--     [ toSql stateRecord.name, toSql stateRecord.ingredients, toSql stateRecord.recipeSteps ]

-- getSteps :: String -> Aff CookingState
-- getSteps recipeName = withConnection $ \conn -> do
--   steps :: Array RecipeStepsDB <- execQuery conn $ Query $ i"SELECT * FROM "recipeStepsTable" WHERE recipeName = '"recipeName"' ORDER BY stepNumber ASC" 

--   guard (not $ Array.null steps) # note (i"No recipe steps associated with the recipe '"recipeName"'" :: String) # liftError

--   let 
--     cookingStateSteps = steps <#> \step ->
--       { completed: false, ordinal: step.stepnumber, description: step.stepdescription }

--   pure { recipe: recipeName, steps: List.fromFoldable cookingStateSteps }

-- getRecipesWithSteps :: Aff $ Array String 
-- getRecipesWithSteps = withConnection $ \conn -> do
--   recipes :: Array {recipename::Maybe String} <- execQuery conn $ Query $ i"SELECT recipeName FROM "recipeStepsTable

--   pure $ Array.catMaybes $ Array.fromFoldable (Set.fromFoldable (recipes <#> _.recipename))
