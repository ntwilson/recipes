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
import Recipes.Backend.CosmosDB (class Container, DELETE_ERROR, ItemID(..), PartitionKey(..), QUERY_ERROR, RawContainer, dbError, deleteViaFind, getContainer, getItem, getPartitionKey, insert, newPartitionKeyDef, pointDelete, readAll)
import Recipes.DataStructures (AppState, Ingredient, RecipeSteps, RecipeIngredients, appStateCodecFields)
import Record as Record
import Type.Proxy (Proxy(..))


type ReadAll m r a = ExceptV (QUERY_ERROR + r) m (Array a)
type Insert m r a = a -> ExceptV (STRING_ERROR + r) m Unit
type Delete m r a = a -> ExceptV (DELETE_ERROR + r) m Unit

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
recipesContainer :: ∀ r m. MonadEffect m => ExceptV (STRING_ERROR + r) m RecipeContainer
recipesContainer = getContainer
  
readAllRecipes :: ∀ m r. MonadAff m => ReadAll m r {name::String}
readAllRecipes = do
  container <- recipesContainer # handleError { stringError: throwError <<< dbError <<< error }
  readAll recipeCodec container
insertRecipe :: ∀ m r. MonadAff m => Insert m r {name::String}
insertRecipe item = do
  container <- recipesContainer
  insert recipeCodec container item

deleteRecipe :: ∀ m r. MonadAff m => Delete m r {name::String}
deleteRecipe item = do
  container <- recipesContainer # handleError {stringError: throwError <<< dbError <<< error }
  pointDelete container (getRecipeID item) (getPartitionKey recipesPartitionKey item) # handleError { error: throwError <<< dbError }

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
ingredientsContainer :: ∀ r m. MonadEffect m => ExceptV (STRING_ERROR + r) m IngredientsContainer
ingredientsContainer = getContainer
readAllIngredients :: ∀ r m. MonadAff m => ReadAll m r Ingredient
readAllIngredients = ingredientsContainer # handleError { stringError: throwError <<< dbError <<< error } >>= readAll ingredientCodec 
insertIngredient :: ∀ m r. MonadAff m => Insert m r Ingredient
insertIngredient item = do
  container <- ingredientsContainer
  insert ingredientCodec container item
deleteIngredient :: ∀ m r. MonadAff m => Delete m r Ingredient
deleteIngredient item = do
  container <- ingredientsContainer # handleError { stringError: throwError <<< dbError <<< error }
  pointDelete container (getIngredientID item) (getPartitionKey ingredientsPartitionKey item) # handleError { error: throwError <<< dbError }

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
recipeIngredientsContainer :: ∀ r m. MonadEffect m => ExceptV (STRING_ERROR + r) m RecipeIngredientsContainer
recipeIngredientsContainer = getContainer

readAllRecipeIngredients :: ∀ m r. MonadAff m => ReadAll m r RecipeIngredients
readAllRecipeIngredients = 
  recipeIngredientsContainer # handleError { stringError: throwError <<< dbError <<< error } >>= readAll recipeIngredientsCodec
insertRecipeIngredients :: ∀ m r. MonadAff m => Insert m r RecipeIngredients
insertRecipeIngredients item = do
  container <- recipeIngredientsContainer
  insert recipeIngredientsCodec container item
deleteRecipeIngredients :: ∀ m r. MonadAff m => Delete m r RecipeIngredients
deleteRecipeIngredients item = do
  container <- recipeIngredientsContainer # handleError { stringError: throwError <<< dbError <<< error }
  deleteViaFind recipeIngredientsCodec equate container item
  where 
  equate = (eq `on` _.recipe) && (eq `on` _.ingredient)

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
recipeStepsContainer :: ∀ r m. MonadEffect m => ExceptV (STRING_ERROR + r) m RecipeStepsContainer
recipeStepsContainer = getContainer

readAllRecipeSteps :: ∀ m r. MonadAff m => ReadAll m r RecipeSteps
readAllRecipeSteps = recipeStepsContainer # handleError { stringError: throwError <<< dbError <<< error } >>= readAll recipeStepsCodec
insertRecipeSteps :: ∀ m r. MonadAff m => Insert m r RecipeSteps
insertRecipeSteps item = do
  container <- recipeStepsContainer
  insert recipeStepsCodec container item
deleteRecipeSteps :: ∀ m r. MonadAff m => Delete m r RecipeSteps
deleteRecipeSteps item = do
  container <- recipeStepsContainer # handleError { stringError: throwError <<< dbError <<< error }
  deleteViaFind recipeStepsCodec equate container item
  where 
  equate = (eq `on` _.recipeName) && (eq `on` _.stepNumber)

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
appStateContainer :: ∀ m r. MonadEffect m => ExceptV (STRING_ERROR + r) m AppStateContainer
appStateContainer = getContainer
appStateDBCodec :: _ -> _
appStateDBCodec ingredients = codec' decoder encoder
  where
  itemIDCodec :: JsonCodec ItemID
  itemIDCodec = unsafeCoerce Codec.string
  codec = appStateCodecFields ingredients # Record.insert (Proxy :: _ "id") itemIDCodec # Codec.Record.object "AppState"
  encoder appState = encode codec $ Record.insert (Proxy :: _ "id") appStateID appState
  decoder json = decode codec json <#> Record.delete (Proxy :: _ "id")

readAppState :: ∀ m r. MonadAff m => List Ingredient -> ExceptV (QUERY_ERROR + r) m (Maybe AppState)
readAppState ingredients = do
  container <- appStateContainer # handleError { stringError: throwError <<< dbError <<< error }
  getItem (appStateDBCodec ingredients) container appStateID appStatePartitionKeyValue
insertAppState :: ∀ m r. MonadAff m => _ -> Insert m r AppState
insertAppState ingredients item = do
  container <- appStateContainer
  insert (appStateDBCodec ingredients) container item

deleteAppState :: ∀ m r. MonadAff m => ExceptV (DELETE_ERROR + r) m Unit
deleteAppState = do
  container <- appStateContainer # handleError { stringError: throwError <<< dbError <<< error }
  pointDelete container appStateID appStatePartitionKeyValue # handleError { error: throwError <<< dbError }

