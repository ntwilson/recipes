module Recipes.Backend.LoadState where

import Backend.Prelude

import Data.Array as Array
import Data.List (List)
import Data.List as List
import Recipes.API (RecipesValue)
import Recipes.Backend.DB (appState, execQuery, execUpdate, ingredient, recipe, recipeIngredients, withConnection)
import Recipes.Backend.StateSerialization (decodeAppState, encodeAppState)
import Recipes.DataStructures (AppState, Ingredient, RecipeIngredients, SerializedAppState)
import Selda (selectFrom)
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
