module Recipes.Backend.DB
  ( ConnectConfig
  , Container
  , CosmosClient
  , Database
  , PartitionKey
  , PartitionKeyDefinition
  , QueryError(..)
  , appStateContainer
  , appStatePartitionKey
  , connectionConfig
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
  , newClient
  , newConnection
  , partitionKeyDef
  , printQueryError
  , query
  , readAllAppStates
  , readAllIngredients
  , readAllRecipeIngredients
  , readAllRecipeSteps
  , readAllRecipes
  , recipeIngredientsPartitionKey
  , recipeStepsContainer
  , recipeStepsPartitionKey
  , recipesContainer
  , recipesPartitionKey
  )
  where

import Backend.Prelude

import Control.Promise (Promise, toAff)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), printJsonDecodeError, (.:))
import Data.Array as Array
import Data.Codec.Argonaut as Codec
import Effect.Exception (message)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Recipes.DataStructures (AppState, Ingredient, RecipeSteps, RecipeIngredients, appStateCodec)
import Unsafe.Coerce (unsafeCoerce)

type ConnectConfig = 
  { endpoint :: String
  , key :: String
  , databaseId :: String
  }

newtype PartitionKey a = PartitionKey { def :: PartitionKeyDefinition, accessor :: a -> String }
newtype PartitionKeyDefinition = PartitionKeyDefinition { paths :: Array String }
-- partition keys must have just a single path
-- https://docs.microsoft.com/en-us/javascript/api/@azure/cosmos/partitionkeydefinition?view=azure-node-latest
newPartitionKeyDef :: String -> PartitionKeyDefinition
newPartitionKeyDef path = PartitionKeyDefinition { paths: [path] }

partitionKeyDef :: ∀ a. PartitionKey a -> PartitionKeyDefinition
partitionKeyDef (PartitionKey { def }) = def

connectionConfig :: ∀ m. MonadEffect m => ExceptT String m ConnectConfig
connectionConfig = do
  key <- env "COSMOS_KEY"
  databaseId <- env "COSMOS_DB"

  pure $ 
    { endpoint: "https://ntw-cosmos.documents.azure.com:443/"
    , key 
    , databaseId
    }

  where 
  env keyname = 
    lift (liftEffect $ lookupEnv keyname) >>= case _ of
      Nothing -> throwError $ i"No "keyname" environment variable found"
      Just key -> pure key

newClient :: ∀ m. MonadEffect m => ExceptT String m CosmosClient
newClient = do
  config <- connectionConfig
  runEffectFn1 cosmosClient config # try # liftEffect # ExceptT # withExceptT message

newConnection :: ∀ m. MonadEffect m => ExceptT String m Database
newConnection = do
  config <- connectionConfig
  runEffectFn1 database config # try # liftEffect # ExceptT # withExceptT message

foreign import data Database :: Type
foreign import data RawContainer :: Type
foreign import data CosmosClient :: Type
foreign import cosmosClient :: ∀ r. EffectFn1 { endpoint :: String, key :: String | r } CosmosClient
foreign import database :: EffectFn1 ConnectConfig Database
foreign import getContainerImpl :: EffectFn2 Database String (RawContainer)

getContainer :: ∀ m. MonadEffect m => String -> Database -> ExceptT String m RawContainer
getContainer containerName database = 
  runEffectFn2 getContainerImpl database containerName # try # liftEffect # ExceptT # withExceptT message

newtype Container a = Container 
  { raw :: RawContainer
  , partitionKey :: PartitionKey a
  }




data QueryError = DBError Error | JsonError JsonDecodeError
printQueryError :: QueryError -> String
printQueryError (DBError err) = message err
printQueryError (JsonError err) = printJsonDecodeError err

type QueryParameter = { name :: String, value :: Json }
foreign import queryImpl :: EffectFn3 RawContainer String (Array QueryParameter) (Promise (Array Json))
query :: ∀ a b m. MonadAff m => DecodeJson b => Container a -> String -> Array QueryParameter -> ExceptT QueryError m (Array b)
query (Container {raw}) query parameters = parseQueryResults decodeClassy $ runEffectFn3 queryImpl raw query parameters

foreign import readAllImpl :: EffectFn1 RawContainer (Promise (Array Json))

readAll :: ∀ a m. MonadAff m => (Json -> Either JsonDecodeError a) -> (Database -> ExceptT String m (Container a)) -> ExceptT QueryError m (Array a)
readAll decode container = do 
  conn <- newConnection # withExceptT (error >>> DBError)
  (Container {raw})  <- container conn # withExceptT (error >>> DBError)
  parseQueryResults (lmap JsonError <<< decode) $ runEffectFn1 readAllImpl raw

decodeClassy :: ∀ a. DecodeJson a => Json -> Either QueryError a
decodeClassy = decodeJson >>> lmap JsonError

codecErrToClassyErr :: Codec.JsonDecodeError -> JsonDecodeError
codecErrToClassyErr = case _ of
  Codec.TypeMismatch str -> TypeMismatch str
  Codec.UnexpectedValue json -> UnexpectedValue json
  Codec.AtIndex i err -> AtIndex i $ codecErrToClassyErr err
  Codec.AtKey k err -> AtKey k $ codecErrToClassyErr err
  Codec.Named name err -> Named name $ codecErrToClassyErr err
  Codec.MissingValue -> MissingValue

parseQueryResults :: ∀ m a. MonadAff m => (Json -> Either QueryError a) -> Effect (Promise (Array Json)) -> ExceptT QueryError m (Array a)
parseQueryResults decode rawResult = do
  results <- effPromiseToAff rawResult # withExceptT DBError
  traverse decode results # except


foreign import insertImpl :: EffectFn2 RawContainer Json (Promise Unit)
insert :: ∀ a m. MonadAff m => (a -> Json) -> (Database -> ExceptT String m (Container a)) -> a -> ExceptT String m Unit
insert encode container item = do
  conn <- newConnection
  (Container {raw}) <- container conn
  runEffectFn2 insertImpl raw (encode item) # effPromiseToAff # withExceptT message


foreign import deleteImpl :: EffectFn3 RawContainer Json String (Promise Unit) 

delete :: ∀ a m. MonadAff m => 
  (Container a -> a -> ExceptT String m Json) -> (Database -> ExceptT String m (Container a)) -> a -> ExceptT String m Unit
delete dbLookup createContainer item = do
  conn <- newConnection
  container@(Container {raw, partitionKey: PartitionKey { accessor }}) <- createContainer conn
  json <- dbLookup container item

  let
    itemID = (unsafeCoerce json).id -- all records in a cosmos database come with an id field
    itemPartitionKey = accessor item

  runEffectFn3 deleteImpl raw itemID itemPartitionKey # effPromiseToAff # withExceptT message

effPromiseToAff :: ∀ m a. MonadAff m => Effect (Promise a) -> ExceptT Error m a
effPromiseToAff eff = do
  promise <- eff # try # liftEffect # ExceptT
  toAff promise # try # liftAff # ExceptT

foreign import getItemImpl :: EffectFn3 RawContainer Json String (Promise Json)

type ReadAll m a = MonadAff m => ExceptT QueryError m (Array a)
type Insert m a = MonadAff m => a -> ExceptT String m Unit
type Delete m a = MonadAff m => a -> ExceptT String m Unit

recipesPartitionKey :: PartitionKey {name :: String}
recipesPartitionKey = PartitionKey { def: newPartitionKeyDef "/id", accessor: _.name }
decodeRecipe :: Json -> Either JsonDecodeError {name::String}
decodeRecipe json = do
  obj <- decodeJson json
  obj .: "id" <#> {name: _}
encodeRecipe :: {name::String} -> Json
encodeRecipe {name} = encodeJson {id: name}
recipesContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container {name::String})
recipesContainer conn = do 
  raw <- getContainer "recipes" conn
  pure $ Container { raw, partitionKey: recipesPartitionKey }
getRecipeFromDB :: ∀ m. MonadAff m => Container {name::String} -> {name::String} -> ExceptT String m Json
getRecipeFromDB (Container {raw}) recipe = 
  runEffectFn3 getItemImpl raw (encodeJson recipe.name) recipe.name # effPromiseToAff # withExceptT message
  
readAllRecipes :: ∀ m. ReadAll m {name::String}
readAllRecipes = readAll decodeRecipe recipesContainer
insertRecipe :: ∀ m. Insert m {name::String}
insertRecipe = insert encodeRecipe recipesContainer
deleteRecipe :: ∀ m. Delete m {name::String}
deleteRecipe = delete getRecipeFromDB recipesContainer 

ingredientsPartitionKey :: PartitionKey Ingredient
ingredientsPartitionKey = PartitionKey { def: newPartitionKeyDef "/id", accessor: _.name }
encodeIngredient :: Ingredient -> Json
encodeIngredient { name, store, section, common } = encodeJson { id: name, store, section, common }
decodeIngredient :: Json -> Either JsonDecodeError Ingredient
decodeIngredient json = do
  ({ id, store, section, common } :: { id::_, store::_, section::_, common::_ }) <- decodeJson json
  pure { name: id, store, section, common }
ingredientsContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container Ingredient)
ingredientsContainer conn = do
  raw <- getContainer "ingredients" conn
  pure $ Container { raw, partitionKey: ingredientsPartitionKey }
getIngredientFromDB :: ∀ m. MonadAff m => Container Ingredient -> Ingredient -> ExceptT String m Json
getIngredientFromDB (Container {raw}) ingredient = 
  runEffectFn3 getItemImpl raw (encodeJson ingredient.name) ingredient.name # effPromiseToAff # withExceptT message

readAllIngredients :: ∀ m. ReadAll m Ingredient
readAllIngredients = readAll decodeIngredient ingredientsContainer
insertIngredient :: ∀ m. Insert m Ingredient
insertIngredient = insert encodeIngredient ingredientsContainer
deleteIngredient :: ∀ m. Delete m Ingredient
deleteIngredient = delete getIngredientFromDB ingredientsContainer

recipeIngredientsPartitionKey :: PartitionKey RecipeIngredients
recipeIngredientsPartitionKey = PartitionKey { def: newPartitionKeyDef "/recipe", accessor: _.recipe }
encodeRecipeIngredients :: RecipeIngredients -> Json
encodeRecipeIngredients = encodeJson
decodeRecipeIngredients :: Json -> Either JsonDecodeError RecipeIngredients
decodeRecipeIngredients = decodeJson
recipeIngredientsContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container RecipeIngredients)
recipeIngredientsContainer conn = do
  raw <- getContainer "recipeIngredients" conn
  pure $ Container { raw, partitionKey: recipeIngredientsPartitionKey }
getRecipeIngredientsFromDB :: ∀ m. MonadAff m => Container RecipeIngredients -> RecipeIngredients -> ExceptT String m Json
getRecipeIngredientsFromDB (Container {raw}) recipeIngredient = do
  steps <- parseQueryResults (lmap JsonError <<< decode) (runEffectFn1 readAllImpl raw) # withExceptT printQueryError
  step <- 
    steps 
    # Array.find (\{decoded} -> recipeIngredient.recipe == decoded.recipe && recipeIngredient.ingredient == decoded.ingredient)
    # note (i"No recipe ingridient "recipeIngredient.ingredient" found for recipe "recipeIngredient.recipe)
    # except

  pure step.json

  where
  decode json = do
    decoded <- decodeRecipeIngredients json
    pure {json, decoded}


readAllRecipeIngredients :: ∀ m. ReadAll m RecipeIngredients
readAllRecipeIngredients = readAll decodeRecipeIngredients recipeIngredientsContainer
insertRecipeIngredients :: ∀ m. Insert m RecipeIngredients
insertRecipeIngredients = insert encodeRecipeIngredients recipeIngredientsContainer
deleteRecipeIngredients :: ∀ m. Delete m RecipeIngredients
deleteRecipeIngredients = delete getRecipeIngredientsFromDB recipeIngredientsContainer

recipeStepsPartitionKey :: PartitionKey RecipeSteps
recipeStepsPartitionKey = PartitionKey { def: newPartitionKeyDef "/recipeName", accessor: _.recipeName }
encodeRecipeSteps :: RecipeSteps -> Json
encodeRecipeSteps = encodeJson
decodeRecipeSteps :: Json -> Either JsonDecodeError RecipeSteps
decodeRecipeSteps = decodeJson
recipeStepsContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container RecipeSteps)
recipeStepsContainer conn = do
  raw <- getContainer "recipeSteps" conn
  pure $ Container { raw, partitionKey: recipeStepsPartitionKey }
getRecipeStepsFromDB :: ∀ m. MonadAff m => Container RecipeSteps -> RecipeSteps -> ExceptT String m Json
getRecipeStepsFromDB (Container {raw}) recipeStep = do
  steps <- parseQueryResults (lmap JsonError <<< decode) (runEffectFn1 readAllImpl raw) # withExceptT printQueryError
  step <- 
    steps 
    # Array.find (\{decoded} -> recipeStep.recipeName == decoded.recipeName && recipeStep.stepNumber == decoded.stepNumber)
    # note (i"No recipe step #"recipeStep.stepNumber" found for recipe "recipeStep.recipeName)
    # except

  pure step.json

  where
  decode json = do
    decoded <- decodeRecipeSteps json
    pure {json, decoded}

readAllRecipeSteps :: ∀ m. ReadAll m RecipeSteps
readAllRecipeSteps = readAll decodeRecipeSteps recipeStepsContainer
insertRecipeSteps :: ∀ m. Insert m RecipeSteps
insertRecipeSteps = insert encodeRecipeSteps recipeStepsContainer
deleteRecipeSteps :: ∀ m. Delete m RecipeSteps
deleteRecipeSteps = delete getRecipeStepsFromDB recipeStepsContainer

appStatePartitionKey :: PartitionKey AppState
appStatePartitionKey = PartitionKey { def: newPartitionKeyDef "/useCase", accessor: show <<< _.useCase }
encodeAppState :: _ -> AppState -> Json
encodeAppState ingredients = Codec.encode $ appStateCodec ingredients
decodeAppState :: _ -> Json -> Either JsonDecodeError AppState
decodeAppState ingredients = Codec.decode (appStateCodec ingredients) >>> lmap codecErrToClassyErr
appStateContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container AppState)
appStateContainer conn = do
  raw <- getContainer "appState" conn
  pure $ Container { raw, partitionKey: appStatePartitionKey }
getAppStateFromDB :: ∀ m. MonadAff m => Container AppState -> AppState -> ExceptT String m Json
getAppStateFromDB (Container {raw}) _ = do
  steps <- parseQueryResults (lmap JsonError <<< decode) (runEffectFn1 readAllImpl raw) # withExceptT printQueryError
  step <- steps # Array.head # note "No app state found" # except
  pure step

  where
  decode json = pure json

readAllAppStates :: ∀ m. _ -> ReadAll m AppState
readAllAppStates ingredients = readAll (decodeAppState ingredients) appStateContainer
insertAppState :: ∀ m. _ -> Insert m AppState
insertAppState ingredients = insert (encodeAppState ingredients) appStateContainer
deleteAppState :: ∀ m. Delete m AppState
deleteAppState = delete getAppStateFromDB appStateContainer
