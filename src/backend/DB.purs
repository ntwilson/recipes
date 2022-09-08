module Recipes.Backend.DB
  ( ConnectConfig
  , Container
  , CosmosClient
  , Database
  , PartitionKeyDefinition
  , QueryError(..)
  , connectionConfig
  , ingredientsContainer
  , insert
  , newClient
  , newConnection
  , newPartitionKeyDef
  , printQueryError
  , query
  , readAll
  , recipeContainer
  , recipeIngredientsContainer
  , recipeStepsContainer
  )
  where

import Backend.Prelude

import Control.Promise (Promise, toAff)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, printJsonDecodeError)
import Effect.Exception (message)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Recipes.DataStructures (Ingredient, RecipeIngredients, RecipeSteps)

type ConnectConfig = 
  { endpoint :: String
  , key :: String
  , databaseId :: String
  }

newtype PartitionKeyDefinition = PartitionKeyDefinition { paths :: Array String }
-- partition keys must have just a single path
-- https://docs.microsoft.com/en-us/javascript/api/@azure/cosmos/partitionkeydefinition?view=azure-node-latest
newPartitionKeyDef :: String -> PartitionKeyDefinition
newPartitionKeyDef path = PartitionKeyDefinition { paths: [path] }

connectionConfig :: ∀ m. MonadEffect m => ExceptT String m ConnectConfig
connectionConfig = do
  key <- env "COSMOS_KEY"
  databaseId <- env "COSMOS_DB"

  pure $ 
    { endpoint: "https://nw-cosmos.documents.azure.com:443/"
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
foreign import data Container :: Type -> Type
foreign import data CosmosClient :: Type
foreign import cosmosClient :: ∀ r. EffectFn1 { endpoint :: String, key :: String | r } CosmosClient
foreign import database :: EffectFn1 ConnectConfig Database
foreign import getContainerImpl :: ∀ a. EffectFn2 Database String (Container a)

getContainer :: ∀ a m. MonadEffect m => String -> Database -> ExceptT String m (Container a)
getContainer containerName database = 
  runEffectFn2 getContainerImpl database containerName # try # liftEffect # ExceptT # withExceptT message

recipeContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container {name::String})
recipeContainer = getContainer "recipes"

ingredientsContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container Ingredient)
ingredientsContainer = getContainer "ingredients"

recipeIngredientsContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container RecipeIngredients)
recipeIngredientsContainer = getContainer "recipeIngredients"

recipeStepsContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container RecipeSteps)
recipeStepsContainer = getContainer "recipeSteps"

type QueryParameter a = { name :: String, value :: a }
data QueryError = DBError Error | JsonError JsonDecodeError
printQueryError :: QueryError -> String
printQueryError (DBError err) = message err
printQueryError (JsonError err) = printJsonDecodeError err

foreign import queryImpl :: ∀ a. Container a -> String -> Array Json -> Effect (Promise (Array Json))
query :: ∀ a m. MonadAff m => DecodeJson a => Container a -> String -> Array Json -> ExceptT QueryError m (Array a)
query container query parameters = parseQueryResults $ queryImpl container query parameters

foreign import readAllImpl :: ∀ a. Container a -> Effect (Promise (Array Json))
readAll :: ∀ a m. MonadAff m => DecodeJson a => Container a -> ExceptT QueryError m (Array a)
readAll container = parseQueryResults $ readAllImpl container

parseQueryResults :: ∀ m a. MonadAff m => DecodeJson a => Effect (Promise (Array Json)) -> ExceptT QueryError m (Array a)
parseQueryResults rawResult = do
  promise <- try rawResult # liftEffect # ExceptT # withExceptT DBError
  results <- toAff promise # try # liftAff # ExceptT # withExceptT DBError

  traverse decodeJson results # except # withExceptT JsonError

foreign import insertImpl :: ∀ a. EffectFn2 (Container a) Json (Promise Unit)
insert :: ∀ a m. MonadAff m => EncodeJson a => Container a -> a -> ExceptT String m Unit
insert container item = do
  promise <- runEffectFn2 insertImpl container (encodeJson item) # try # liftEffect # ExceptT # withExceptT message
  toAff promise # try # liftAff # ExceptT # withExceptT message

