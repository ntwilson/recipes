module Recipes.Backend.LoadState where

import Backend.Prelude

import Control.MonadZero (guard)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Set as Set
import Recipes.API (RecipesValue)
import Recipes.Backend.DB (appState, execQuery, execUpdate, ingredient, recipe, recipeIngredients, recipeSteps, withConnection)
import Recipes.DataStructures (AppState, Ingredient, RecipeIngredients, SerializedAppState, CookingState)
import Recipes.StateSerialization (decodeAppState, encodeAppState)
import Selda (asc, orderBy, restrict, selectFrom, (.==))
import Selda as Selda


allRecipes :: Aff RecipesValue
allRecipes = withConnection $ \conn ->
  ( execQuery conn $ selectFrom recipe (\{name} -> pure {name})) <##> _.name

allIngredients :: Aff $ List Ingredient
allIngredients = withConnection $ \conn ->
  List.fromFoldable <$> (execQuery conn $ selectFrom ingredient pure)

allRecipeIngredients :: Aff $ List RecipeIngredients 
allRecipeIngredients = withConnection $ \conn ->
  List.fromFoldable <$> (execQuery conn $ selectFrom recipeIngredients pure)

getSerializedState :: Aff SerializedAppState
getSerializedState = withConnection $ \conn -> do
  serializedRecords <- execQuery conn $ selectFrom appState pure 
  Array.head serializedRecords # note "No appState record found in the database" # liftError

getState :: Aff AppState
getState = withConnection $ \conn -> do
  ingredients <- execQuery conn $ selectFrom ingredient pure
  serializedRecords <- execQuery conn $ selectFrom appState pure 
  serialized <- Array.head serializedRecords # note "No appState record found in the database" # liftError
  decodeAppState (List.fromFoldable ingredients) serialized

setState :: AppState -> Aff Unit
setState state = withConnection $ \conn -> do
  let stateRecord = encodeAppState state
  execUpdate conn appState (const $ Selda.lit true)  
    (const {name: Selda.lit stateRecord.name, ingredients: Selda.lit stateRecord.ingredients, recipeSteps: Selda.lit stateRecord.recipeSteps}) 

getSteps :: String -> Aff CookingState
getSteps recipeName = withConnection $ \conn -> do
  steps <- execQuery conn $ selectFrom recipeSteps \step -> do
    restrict $ step.recipeName .== Selda.lit recipeName
    orderBy asc step.stepNumber 
    pure step

  guard (not $ Array.null steps) # note (i"No recipe steps associated with the recipe '"recipeName"'" :: String) # liftError

  let 
    cookingStateSteps = steps <#> \step ->
      { completed: false, ordinal: step.stepNumber, description: step.stepDescription }

  pure { recipe: recipeName, steps: List.fromFoldable cookingStateSteps }

getRecipesWithSteps :: Aff $ Array String 
getRecipesWithSteps = withConnection $ \conn -> do
  recipes <- execQuery conn $ selectFrom recipeSteps \{recipeName} -> pure {recipeName}

  pure $ Array.fromFoldable (Set.fromFoldable (recipes <#> _.recipeName))
