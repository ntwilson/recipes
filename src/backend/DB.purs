module Recipes.Backend.DB
  ( AppStateContainer(..)
  , IngredientsContainer(..)
  , RecipeContainer(..)
  , RecipeIngredientsContainer(..)
  , RecipeStepsContainer(..)
  , appStateContainer
  , appStatePartitionKey
  , deleteAppState
  , deleteIngredient
  , deleteRecipe
  , deleteRecipeIngredients
  , deleteRecipeSteps
  , ingredientsContainer
  , ingredientsPartitionKey
  , insertAppState
  , insertIngredient
  , insertRecipe
  , insertRecipeIngredients
  , insertRecipeSteps
  , readAllIngredients
  , readAllRecipeIngredients
  , readAllRecipeSteps
  , readAllRecipes
  , readAppState
  , recipeIngredientsPartitionKey
  , recipeStepsCodec
  , recipeStepsContainer
  , recipeStepsPartitionKey
  , recipesContainer
  , recipesPartitionKey
  )
  where

import Backend.Prelude

import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Compat as Codec.Compat
import Data.Codec.Argonaut.Record as Codec.Record
import Data.List (List)
import Data.Newtype (class Newtype)
import Recipes.Backend.CosmosDB (class Container, DELETE_ERROR, ItemID(..), PartitionKey(..), QUERY_ERROR, RawContainer, _dbError, deleteViaFind, getContainer, getItem, getPartitionKey, insert, newPartitionKeyDef, pointDelete, readAll)
import Recipes.DataStructures (AppState, Ingredient, RecipeSteps, RecipeIngredients, appStateCodecFields)
import Record as Record
import Type.Proxy (Proxy(..))


type ReadAll r a = Run (AFFECT + QUERY_ERROR + r) (Array a)
type Insert r a = a -> Run (AFFECT + EXCEPT String + r) Unit
type Delete r a = a -> Run (AFFECT + DELETE_ERROR + r) Unit

recipesPartitionKey :: PartitionKey RecipeContainer {name :: String}
recipesPartitionKey = PartitionKey { def: newPartitionKeyDef "/id", accessor: _.name }
getRecipeID :: {name::String} -> ItemID
getRecipeID {name} = ItemID name
recipeCodec :: JsonCodec {name::String}
recipeCodec = codec' decoder encoder
  where
  codec = Codec.Record.object "Recipe" {id: Codec.string}
  encoder {name} = encode codec {id: name}
  decoder json = decode codec json <#> Record.rename (Proxy :: _ "id") (Proxy :: _ "name")
newtype RecipeContainer = RecipeContainer RawContainer
derive instance Newtype RecipeContainer _
instance Container RecipeContainer {name :: String} where 
  partitionKey = recipesPartitionKey
  containerName _ = "recipes"
recipesContainer :: ∀ r. Run (EFFECT + EXCEPT String + r) RecipeContainer
recipesContainer = getContainer
  
readAllRecipes :: ∀ m. ReadAll m {name::String}
readAllRecipes = do
  container <- recipesContainer # withExceptAt _except _dbError error
  readAll recipeCodec container
insertRecipe :: ∀ m. Insert m {name::String}
insertRecipe item = do
  container <- recipesContainer
  insert recipeCodec container item

deleteRecipe :: ∀ m. Delete m {name::String}
deleteRecipe item = do
  container <- recipesContainer # withExceptAt _except _dbError error
  pointDelete container (getRecipeID item) (getPartitionKey recipesPartitionKey item) # moveExcept _except _dbError

ingredientsPartitionKey :: PartitionKey IngredientsContainer Ingredient
ingredientsPartitionKey = PartitionKey { def: newPartitionKeyDef "/id", accessor: _.name }
getIngredientID :: Ingredient -> ItemID
getIngredientID {name} = ItemID name
ingredientCodec :: JsonCodec Ingredient
ingredientCodec = codec' decoder encoder
  where
  codec = Codec.Record.object "Ingredient" 
    { id: Codec.string, store: Codec.string, section: Codec.Compat.maybe Codec.string, common: Codec.boolean }
  encoder {name, store, section, common} = encode codec {id: name, store, section, common}
  decoder json = decode codec json <#> Record.rename (Proxy :: _ "id") (Proxy :: _ "name")

newtype IngredientsContainer = IngredientsContainer RawContainer
derive instance Newtype IngredientsContainer _
instance Container IngredientsContainer Ingredient where 
  partitionKey = ingredientsPartitionKey
  containerName _ = "ingredients"
ingredientsContainer :: ∀ r. Run (EFFECT + EXCEPT String + r) IngredientsContainer
ingredientsContainer = getContainer
readAllIngredients :: ∀ r. ReadAll r Ingredient
readAllIngredients = readAll ingredientCodec =<< withExceptAt _except _dbError error ingredientsContainer
insertIngredient :: ∀ m. Insert m Ingredient
insertIngredient item = do
  container <- ingredientsContainer
  insert ingredientCodec container item
deleteIngredient :: ∀ m. Delete m Ingredient
deleteIngredient item = do
  container <- ingredientsContainer # withExceptAt _except _dbError error
  pointDelete container (getIngredientID item) (getPartitionKey ingredientsPartitionKey item) # moveExcept _except _dbError

recipeIngredientsPartitionKey :: PartitionKey RecipeIngredientsContainer RecipeIngredients
recipeIngredientsPartitionKey = PartitionKey { def: newPartitionKeyDef "/recipe", accessor: _.recipe }
recipeIngredientsCodec :: JsonCodec RecipeIngredients
recipeIngredientsCodec = Codec.Record.object "RecipeIngredients"
  { recipe: Codec.string, ingredient: Codec.string, quantity: Codec.number, units: Codec.Compat.maybe Codec.string }
newtype RecipeIngredientsContainer = RecipeIngredientsContainer RawContainer
derive instance Newtype RecipeIngredientsContainer _
instance Container RecipeIngredientsContainer RecipeIngredients where 
  partitionKey = recipeIngredientsPartitionKey
  containerName _ = "recipeIngredients"
recipeIngredientsContainer :: ∀ r. Run (EFFECT + EXCEPT String + r) RecipeIngredientsContainer
recipeIngredientsContainer = getContainer

readAllRecipeIngredients :: ∀ m. ReadAll m RecipeIngredients
readAllRecipeIngredients = readAll recipeIngredientsCodec =<< withExceptAt _except _dbError error recipeIngredientsContainer
insertRecipeIngredients :: ∀ m. Insert m RecipeIngredients
insertRecipeIngredients item = do
  container <- recipeIngredientsContainer
  insert recipeIngredientsCodec container item
deleteRecipeIngredients :: ∀ m. Delete m RecipeIngredients
deleteRecipeIngredients item = do
  container <- recipeIngredientsContainer # withExceptAt _except _dbError error
  deleteViaFind recipeIngredientsCodec equate container item
  where 
  equate = equating _.recipe && equating _.ingredient

recipeStepsPartitionKey :: PartitionKey RecipeStepsContainer RecipeSteps
recipeStepsPartitionKey = PartitionKey { def: newPartitionKeyDef "/recipeName", accessor: _.recipeName }
recipeStepsCodec :: JsonCodec RecipeSteps
recipeStepsCodec = Codec.Record.object "RecipeSteps" 
  { recipeName: Codec.string, stepNumber: Codec.int, stepDescription: Codec.string }
newtype RecipeStepsContainer = RecipeStepsContainer RawContainer
derive instance Newtype RecipeStepsContainer _
instance Container RecipeStepsContainer RecipeSteps where 
  partitionKey = recipeStepsPartitionKey
  containerName _ = "recipeSteps"
recipeStepsContainer :: ∀ r. Run (EFFECT + EXCEPT String + r) RecipeStepsContainer
recipeStepsContainer = getContainer

readAllRecipeSteps :: ∀ m. ReadAll m RecipeSteps
readAllRecipeSteps = readAll recipeStepsCodec =<< withExceptAt _except _dbError error recipeStepsContainer
insertRecipeSteps :: ∀ m. Insert m RecipeSteps
insertRecipeSteps item = do
  container <- recipeStepsContainer
  insert recipeStepsCodec container item
deleteRecipeSteps :: ∀ m. Delete m RecipeSteps
deleteRecipeSteps item = do
  container <- recipeStepsContainer # withExceptAt _except _dbError error
  deleteViaFind recipeStepsCodec equate container item
  where 
  equate = equating _.recipeName && equating _.stepNumber

appStatePartitionKeyValue :: String
appStatePartitionKeyValue = "singleton"
appStateID :: ItemID
appStateID = ItemID appStatePartitionKeyValue
appStatePartitionKey :: PartitionKey AppStateContainer AppState
appStatePartitionKey = PartitionKey { def: newPartitionKeyDef "/id", accessor: const appStatePartitionKeyValue }
newtype AppStateContainer = AppStateContainer RawContainer
derive instance Newtype AppStateContainer _
instance Container AppStateContainer AppState where 
  partitionKey = appStatePartitionKey
  containerName _ = "appState"
appStateContainer :: ∀ r. Run (EFFECT + EXCEPT String + r) AppStateContainer
appStateContainer = getContainer
appStateDBCodec :: _ -> _
appStateDBCodec ingredients = codec' decoder encoder
  where
  itemIDCodec :: JsonCodec ItemID
  itemIDCodec = unsafeCoerce Codec.string
  codec = appStateCodecFields ingredients # Record.insert (Proxy :: _ "id") itemIDCodec # Codec.Record.object "AppState"
  encoder appState = encode codec $ Record.insert (Proxy :: _ "id") appStateID appState
  decoder json = decode codec json <#> Record.delete (Proxy :: _ "id")

readAppState :: ∀ r. List Ingredient -> Run (AFFECT + QUERY_ERROR + r) (Maybe AppState)
readAppState ingredients = do
  container <- withExceptAt _except _dbError error appStateContainer
  getItem (appStateDBCodec ingredients) container appStateID appStatePartitionKeyValue
insertAppState :: ∀ r. _ -> Insert r AppState
insertAppState ingredients item = do
  container <- appStateContainer
  insert (appStateDBCodec ingredients) container item

deleteAppState :: ∀ r. Run (AFFECT + DELETE_ERROR + r) Unit
deleteAppState = do
  container <- appStateContainer # withExceptAt _except _dbError error
  pointDelete container appStateID appStatePartitionKeyValue # moveExcept _except _dbError

