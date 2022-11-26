module Recipes.Backend.DB
  ( appStateContainer
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
  , readAppState
  , readAllIngredients
  , readAllRecipeIngredients
  , readAllRecipeSteps
  , readAllRecipes
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
import Recipes.Backend.CosmosDB (Container, DeleteError(..), ItemID(..), PartitionKey(..), QueryError(..), deleteViaFind, getContainer, getItem, getPartitionKey, insert, newPartitionKeyDef, pointDelete, readAll)
import Recipes.DataStructures (AppState, Ingredient, RecipeIngredients, RecipeSteps, appStateCodecFields)
import Record as Record
import Type.Proxy (Proxy(..))


type ReadAll m a = MonadAff m => ExceptT QueryError m (Array a)
type Insert m a = MonadAff m => a -> ExceptT String m Unit
type Delete m a = MonadAff m => a -> ExceptT DeleteError m Unit

recipesPartitionKey :: PartitionKey {name :: String}
recipesPartitionKey = PartitionKey { def: newPartitionKeyDef "/id", accessor: _.name }
getRecipeID :: {name::String} -> ItemID
getRecipeID {name} = ItemID name
recipeCodec :: JsonCodec {name::String}
recipeCodec = basicCodec decoder encoder
  where
  codec = Codec.Record.object "Recipe" {id: Codec.string}
  encoder {name} = encode codec {id: name}
  decoder json = decode codec json <#> Record.rename (Proxy :: _ "id") (Proxy :: _ "name")
recipesContainer :: ∀ m. MonadEffect m => ExceptT String m (Container {name::String})
recipesContainer = getContainer "recipes" recipesPartitionKey
  
readAllRecipes :: ∀ m. ReadAll m {name::String}
readAllRecipes = do
  container <- recipesContainer # withExceptT (error >>> DBError)
  readAll recipeCodec container
insertRecipe :: ∀ m. Insert m {name::String}
insertRecipe item = do
  container <- recipesContainer
  insert recipeCodec container item

deleteRecipe :: ∀ m. Delete m {name::String}
deleteRecipe item = do
  container <- recipesContainer # withExceptT (Err <<< DBError <<< error)
  pointDelete container (getRecipeID item) (getPartitionKey recipesPartitionKey item) # withExceptT (Err <<< DBError)

ingredientsPartitionKey :: PartitionKey Ingredient
ingredientsPartitionKey = PartitionKey { def: newPartitionKeyDef "/id", accessor: _.name }
getIngredientID :: Ingredient -> ItemID
getIngredientID {name} = ItemID name
ingredientCodec :: JsonCodec Ingredient
ingredientCodec = basicCodec decoder encoder
  where
  codec = Codec.Record.object "Ingredient" 
    { id: Codec.string, store: Codec.string, section: Codec.Compat.maybe Codec.string, common: Codec.boolean }
  encoder {name, store, section, common} = encode codec {id: name, store, section, common}
  decoder json = decode codec json <#> Record.rename (Proxy :: _ "id") (Proxy :: _ "name")

ingredientsContainer :: ∀ m. MonadEffect m => ExceptT String m (Container Ingredient)
ingredientsContainer = getContainer "ingredients" ingredientsPartitionKey
readAllIngredients :: ∀ m. ReadAll m Ingredient
readAllIngredients = readAll ingredientCodec =<< withExceptT (DBError <<< error) ingredientsContainer
insertIngredient :: ∀ m. Insert m Ingredient
insertIngredient item = do
  container <- ingredientsContainer
  insert ingredientCodec container item
deleteIngredient :: ∀ m. Delete m Ingredient
deleteIngredient item = do
  container <- ingredientsContainer # withExceptT (Err <<< DBError <<< error)
  pointDelete container (getIngredientID item) (getPartitionKey ingredientsPartitionKey item) # withExceptT (Err <<< DBError)

recipeIngredientsPartitionKey :: PartitionKey RecipeIngredients
recipeIngredientsPartitionKey = PartitionKey { def: newPartitionKeyDef "/recipe", accessor: _.recipe }
recipeIngredientsCodec :: JsonCodec RecipeIngredients
recipeIngredientsCodec = Codec.Record.object "RecipeIngredients"
  { recipe: Codec.string, ingredient: Codec.string, quantity: Codec.number, units: Codec.Compat.maybe Codec.string }
recipeIngredientsContainer :: ∀ m. MonadEffect m => ExceptT String m (Container RecipeIngredients)
recipeIngredientsContainer = getContainer "recipeIngredients" recipeIngredientsPartitionKey

readAllRecipeIngredients :: ∀ m. ReadAll m RecipeIngredients
readAllRecipeIngredients = readAll recipeIngredientsCodec =<< withExceptT (error >>> DBError) recipeIngredientsContainer
insertRecipeIngredients :: ∀ m. Insert m RecipeIngredients
insertRecipeIngredients item = do
  container <- recipeIngredientsContainer
  insert recipeIngredientsCodec container item
deleteRecipeIngredients :: ∀ m. Delete m RecipeIngredients
deleteRecipeIngredients item = do
  container <- recipeIngredientsContainer # withExceptT (Err <<< DBError <<< error)
  deleteViaFind recipeIngredientsCodec equate container item
  where 
  equate = equating _.recipe && equating _.ingredient

recipeStepsPartitionKey :: PartitionKey RecipeSteps
recipeStepsPartitionKey = PartitionKey { def: newPartitionKeyDef "/recipeName", accessor: _.recipeName }
recipeStepsCodec :: JsonCodec RecipeSteps
recipeStepsCodec = Codec.Record.object "RecipeSteps" 
  { recipeName: Codec.string, stepNumber: Codec.int, stepDescription: Codec.string }
recipeStepsContainer :: ∀ m. MonadEffect m => ExceptT String m (Container RecipeSteps)
recipeStepsContainer = getContainer "recipeSteps" recipeStepsPartitionKey 

readAllRecipeSteps :: ∀ m. ReadAll m RecipeSteps
readAllRecipeSteps = readAll recipeStepsCodec =<< withExceptT (error >>> DBError) recipeStepsContainer
insertRecipeSteps :: ∀ m. Insert m RecipeSteps
insertRecipeSteps item = do
  container <- recipeStepsContainer
  insert recipeStepsCodec container item
deleteRecipeSteps :: ∀ m. Delete m RecipeSteps
deleteRecipeSteps item = do
  container <- recipeStepsContainer # withExceptT (Err <<< DBError <<< error)
  deleteViaFind recipeStepsCodec equate container item
  where 
  equate = equating _.recipeName && equating _.stepNumber -- b.recipeName == a.recipeName && b.stepNumber == a.stepNumber

appStatePartitionKeyValue :: String
appStatePartitionKeyValue = "singleton"
appStateID :: ItemID
appStateID = ItemID appStatePartitionKeyValue
appStatePartitionKey :: PartitionKey AppState
appStatePartitionKey = PartitionKey { def: newPartitionKeyDef "/id", accessor: const appStatePartitionKeyValue }
appStateContainer :: ∀ m. MonadEffect m => ExceptT String m (Container AppState)
appStateContainer = getContainer "appState" appStatePartitionKey
appStateDBCodec :: _ -> _
appStateDBCodec ingredients = basicCodec decoder encoder
  where
  itemIDCodec :: JsonCodec ItemID
  itemIDCodec = unsafeCoerce Codec.string
  codec = appStateCodecFields ingredients # Record.insert (Proxy :: _ "id") itemIDCodec # Codec.Record.object "AppState"
  encoder appState = encode codec $ Record.insert (Proxy :: _ "id") appStateID appState
  decoder json = decode codec json <#> Record.delete (Proxy :: _ "id")

readAppState :: ∀ m. MonadAff m => List Ingredient -> ExceptT QueryError m (Maybe AppState)
readAppState ingredients = do
  container <- withExceptT (error >>> DBError) appStateContainer
  getItem (appStateDBCodec ingredients) container appStateID appStatePartitionKeyValue
insertAppState :: ∀ m. _ -> Insert m AppState
insertAppState ingredients item = do
  container <- appStateContainer
  insert (appStateDBCodec ingredients) container item

deleteAppState :: ∀ m. MonadAff m => ExceptT DeleteError m Unit
deleteAppState = do
  container <- appStateContainer # withExceptT (Err <<< DBError <<< error)
  pointDelete container appStateID appStatePartitionKeyValue # withExceptT (Err <<< DBError)
