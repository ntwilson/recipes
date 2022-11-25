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
  , recipeStepsCodec
  , recipeStepsContainer
  , recipeStepsPartitionKey
  , recipesContainer
  , recipesPartitionKey
  )
  where

import Backend.Prelude

import Control.Promise (Promise, toAff)
import Data.Array as Array
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Compat as Codec.Compat
import Data.Codec.Argonaut.Record as Codec.Record
import Recipes.DataStructures (AppState, Ingredient, RecipeSteps, RecipeIngredients, appStateCodec)
import Record as Record
import Type.Proxy (Proxy(..))

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

catchEffect :: ∀ m a. MonadEffect m => Effect a -> ExceptT String m a
catchEffect = try >>> liftEffect >>> ExceptT >>> withExceptT message

newClient :: ∀ m. MonadEffect m => ExceptT String m CosmosClient
newClient = do
  config <- connectionConfig
  runEffectFn1 cosmosClient config # catchEffect

newConnection :: ∀ m. MonadEffect m => ExceptT String m Database
newConnection = do
  config <- connectionConfig
  runEffectFn1 database config # catchEffect

foreign import data Database :: Type
foreign import data RawContainer :: Type
foreign import data CosmosClient :: Type
foreign import cosmosClient :: ∀ r. EffectFn1 { endpoint :: String, key :: String | r } CosmosClient
foreign import database :: EffectFn1 ConnectConfig Database
foreign import getContainerImpl :: EffectFn2 Database String (RawContainer)

getContainer :: ∀ m. MonadEffect m => String -> Database -> ExceptT String m RawContainer
getContainer containerName database = 
  runEffectFn2 getContainerImpl database containerName # catchEffect

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
query :: ∀ a b m. MonadAff m => JsonCodec b -> Container a -> String -> Array QueryParameter -> ExceptT QueryError m (Array b)
query codec (Container {raw}) query parameters = parseQueryResults codec $ runEffectFn3 queryImpl raw query parameters

foreign import readAllImpl :: EffectFn1 RawContainer (Promise (Array Json))

readAll :: ∀ a m. MonadAff m => JsonCodec a -> (Database -> ExceptT String m (Container a)) -> ExceptT QueryError m (Array a)
readAll codec container = do 
  conn <- newConnection # withExceptT (error >>> DBError)
  (Container {raw})  <- container conn # withExceptT (error >>> DBError)
  parseQueryResults codec $ runEffectFn1 readAllImpl raw

parseQueryResults :: ∀ m a. MonadAff m => JsonCodec a -> Effect (Promise (Array Json)) -> ExceptT QueryError m (Array a)
parseQueryResults codec rawResult = do
  results <- effPromiseToAff rawResult # withExceptT DBError
  traverse (decode codec >>> lmap JsonError) results # except


foreign import insertImpl :: EffectFn2 RawContainer Json (Promise Unit)
insert :: ∀ a m. MonadAff m => JsonCodec a -> (Database -> ExceptT String m (Container a)) -> a -> ExceptT String m Unit
insert codec container item = do
  conn <- newConnection
  (Container {raw}) <- container conn
  runEffectFn2 insertImpl raw (encode codec item) # effPromiseToAff # withExceptT message

-- all records in a cosmos database come with an id field.
getID :: Json -> String
getID json = (unsafeCoerce json).id

foreign import deleteImpl :: EffectFn3 RawContainer String String (Promise Unit) 

delete :: ∀ a m. MonadAff m => 
  (Container a -> a -> ExceptT String m Json) -> (Database -> ExceptT String m (Container a)) -> a -> ExceptT String m Unit
delete dbLookup createContainer item = do
  conn <- newConnection
  container@(Container {raw, partitionKey: PartitionKey { accessor }}) <- createContainer conn
  json <- dbLookup container item

  let
    itemID = getID json
    itemPartitionKey = accessor item

  runEffectFn3 deleteImpl raw itemID itemPartitionKey # effPromiseToAff # withExceptT message

effPromiseToAff :: ∀ m a. MonadAff m => Effect (Promise a) -> ExceptT Error m a
effPromiseToAff eff = do
  promise <- eff # try # liftEffect # ExceptT
  toAff promise # try # liftAff # ExceptT

foreign import getItemImpl :: EffectFn3 RawContainer String String (Promise Json)

type ReadAll m a = MonadAff m => ExceptT QueryError m (Array a)
type Insert m a = MonadAff m => a -> ExceptT String m Unit
type Delete m a = MonadAff m => a -> ExceptT String m Unit

recipesPartitionKey :: PartitionKey {name :: String}
recipesPartitionKey = PartitionKey { def: newPartitionKeyDef "/id", accessor: _.name }
getRecipeID :: {name::String} -> String
getRecipeID = getID <<< encode recipeCodec
recipeCodec :: JsonCodec {name::String}
recipeCodec = basicCodec decoder encoder
  where
  codec = Codec.Record.object "Recipe" {id: Codec.string}
  encoder {name} = encode codec {id: name}
  decoder json = decode codec json <#> Record.rename (Proxy :: _ "id") (Proxy :: _ "name")
recipesContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container {name::String})
recipesContainer conn = do 
  raw <- getContainer "recipes" conn
  pure $ Container { raw, partitionKey: recipesPartitionKey }
getRecipeFromDB :: ∀ m. MonadAff m => Container {name::String} -> {name::String} -> ExceptT String m Json
getRecipeFromDB (Container {raw, partitionKey: PartitionKey {accessor}}) recipe = 
  runEffectFn3 getItemImpl raw (getRecipeID recipe) (accessor recipe) # effPromiseToAff # withExceptT message
  
readAllRecipes :: ∀ m. ReadAll m {name::String}
readAllRecipes = readAll recipeCodec recipesContainer
insertRecipe :: ∀ m. Insert m {name::String}
insertRecipe = insert recipeCodec recipesContainer
deleteRecipe :: ∀ m. Delete m {name::String}
deleteRecipe = delete getRecipeFromDB recipesContainer 

ingredientsPartitionKey :: PartitionKey Ingredient
ingredientsPartitionKey = PartitionKey { def: newPartitionKeyDef "/id", accessor: _.name }
getIngredientID :: Ingredient -> String
getIngredientID = getID <<< encode ingredientCodec
ingredientCodec :: JsonCodec Ingredient
ingredientCodec = basicCodec decoder encoder
  where
  codec = Codec.Record.object "Ingredient" 
    { id: Codec.string, store: Codec.string, section: Codec.Compat.maybe Codec.string, common: Codec.boolean }
  encoder {name, store, section, common} = encode codec {id: name, store, section, common}
  decoder json = decode codec json <#> Record.rename (Proxy :: _ "id") (Proxy :: _ "name")

ingredientsContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container Ingredient)
ingredientsContainer conn = do
  raw <- getContainer "ingredients" conn
  pure $ Container { raw, partitionKey: ingredientsPartitionKey }
getIngredientFromDB :: ∀ m. MonadAff m => Container Ingredient -> Ingredient -> ExceptT String m Json
getIngredientFromDB (Container {raw, partitionKey: PartitionKey {accessor}}) ingredient = 
  runEffectFn3 getItemImpl raw (getIngredientID ingredient) (accessor ingredient) # effPromiseToAff # withExceptT message

readAllIngredients :: ∀ m. ReadAll m Ingredient
readAllIngredients = readAll ingredientCodec ingredientsContainer
insertIngredient :: ∀ m. Insert m Ingredient
insertIngredient = insert ingredientCodec ingredientsContainer
deleteIngredient :: ∀ m. Delete m Ingredient
deleteIngredient = delete getIngredientFromDB ingredientsContainer

recipeIngredientsPartitionKey :: PartitionKey RecipeIngredients
recipeIngredientsPartitionKey = PartitionKey { def: newPartitionKeyDef "/recipe", accessor: _.recipe }
recipeIngredientsCodec :: JsonCodec RecipeIngredients
recipeIngredientsCodec = Codec.Record.object "RecipeIngredients"
  { recipe: Codec.string, ingredient: Codec.string, quantity: Codec.number, units: Codec.Compat.maybe Codec.string }
recipeIngredientsContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container RecipeIngredients)
recipeIngredientsContainer conn = do
  raw <- getContainer "recipeIngredients" conn
  pure $ Container { raw, partitionKey: recipeIngredientsPartitionKey }
getRecipeIngredientsFromDB :: ∀ m. MonadAff m => Container RecipeIngredients -> RecipeIngredients -> ExceptT String m Json
getRecipeIngredientsFromDB (Container {raw}) recipeIngredient = do
  steps <- runEffectFn1 readAllImpl raw # parseQueryResults codec # withExceptT printQueryError
  step <- 
    steps 
    # Array.find (\{decoded} -> recipeIngredient.recipe == decoded.recipe && recipeIngredient.ingredient == decoded.ingredient)
    # note (i"No recipe ingridient "recipeIngredient.ingredient" found for recipe "recipeIngredient.recipe)
    # except

  pure step.json

  where
  codec = basicCodec decoder encoder
  decoder json = do
    decoded <- decode recipeIngredientsCodec json
    pure {json, decoded}
  encoder {json} = json


readAllRecipeIngredients :: ∀ m. ReadAll m RecipeIngredients
readAllRecipeIngredients = readAll recipeIngredientsCodec recipeIngredientsContainer
insertRecipeIngredients :: ∀ m. Insert m RecipeIngredients
insertRecipeIngredients = insert recipeIngredientsCodec recipeIngredientsContainer
deleteRecipeIngredients :: ∀ m. Delete m RecipeIngredients
deleteRecipeIngredients = delete getRecipeIngredientsFromDB recipeIngredientsContainer

recipeStepsPartitionKey :: PartitionKey RecipeSteps
recipeStepsPartitionKey = PartitionKey { def: newPartitionKeyDef "/recipeName", accessor: _.recipeName }
recipeStepsCodec :: JsonCodec RecipeSteps
recipeStepsCodec = Codec.Record.object "RecipeSteps" 
  { recipeName: Codec.string, stepNumber: Codec.int, stepDescription: Codec.string }
recipeStepsContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container RecipeSteps)
recipeStepsContainer conn = do
  raw <- getContainer "recipeSteps" conn
  pure $ Container { raw, partitionKey: recipeStepsPartitionKey }
getRecipeStepsFromDB :: ∀ m. MonadAff m => Container RecipeSteps -> RecipeSteps -> ExceptT String m Json
getRecipeStepsFromDB (Container {raw}) recipeStep = do
  steps <- parseQueryResults codec (runEffectFn1 readAllImpl raw) # withExceptT printQueryError
  step <- 
    steps 
    # Array.find (\{decoded} -> recipeStep.recipeName == decoded.recipeName && recipeStep.stepNumber == decoded.stepNumber)
    # note (i"No recipe step #"recipeStep.stepNumber" found for recipe "recipeStep.recipeName)
    # except

  pure step.json

  where
  codec = basicCodec decoder encoder
  decoder json = do
    decoded <- decode recipeStepsCodec json
    pure {json, decoded}
  encoder {json} = json

readAllRecipeSteps :: ∀ m. ReadAll m RecipeSteps
readAllRecipeSteps = readAll recipeStepsCodec recipeStepsContainer
insertRecipeSteps :: ∀ m. Insert m RecipeSteps
insertRecipeSteps = insert recipeStepsCodec recipeStepsContainer
deleteRecipeSteps :: ∀ m. Delete m RecipeSteps
deleteRecipeSteps = delete getRecipeStepsFromDB recipeStepsContainer

appStatePartitionKey :: PartitionKey AppState
appStatePartitionKey = PartitionKey { def: newPartitionKeyDef "/useCase", accessor: show <<< _.useCase }
appStateContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container AppState)
appStateContainer conn = do
  raw <- getContainer "appState" conn
  pure $ Container { raw, partitionKey: appStatePartitionKey }
getAppStateFromDB :: ∀ m. MonadAff m => Container AppState -> AppState -> ExceptT String m Json
getAppStateFromDB (Container {raw}) _ = do
  steps <- parseQueryResults Codec.json (runEffectFn1 readAllImpl raw) # withExceptT printQueryError
  step <- steps # Array.head # note "No app state found" # except
  pure step

readAllAppStates :: ∀ m. _ -> ReadAll m AppState
readAllAppStates ingredients = readAll (appStateCodec ingredients) appStateContainer
insertAppState :: ∀ m. _ -> Insert m AppState
insertAppState ingredients = insert (appStateCodec ingredients) appStateContainer
deleteAppState :: ∀ m. Delete m AppState
deleteAppState = delete getAppStateFromDB appStateContainer
