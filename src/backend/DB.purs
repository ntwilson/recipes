module Recipes.Backend.DB
  ( ConnectConfig
  , Container
  , CosmosClient
  , Database
  , PartitionKeyDefinition
  , QueryError(..)
  , appStateContainer
  , connectionConfig
  , delete
  , deleteWith
  , ingredientsContainer
  , insert
  , newClient
  , newConnection
  , newPartitionKeyDef
  , printQueryError
  , query
  , readAll
  , readAllWith
  , recipeContainer
  , recipeIngredientsContainer
  , recipeStepsContainer
  )
  where

import Backend.Prelude

import Control.Promise (Promise, toAff)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), printJsonDecodeError)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec
import Effect.Exception (message)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Recipes.DataStructures (Ingredient, RecipeIngredients, RecipeSteps, AppState)

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

appStateContainer :: ∀ m. MonadEffect m => Database -> ExceptT String m (Container AppState)
appStateContainer = getContainer "appState"

data QueryError = DBError Error | JsonError JsonDecodeError
printQueryError :: QueryError -> String
printQueryError (DBError err) = message err
printQueryError (JsonError err) = printJsonDecodeError err

type QueryParameter = { name :: String, value :: Json }
foreign import queryImpl :: ∀ a. EffectFn3 (Container a) String (Array QueryParameter) (Promise (Array Json))
query :: ∀ a m. MonadAff m => DecodeJson a => Container a -> String -> Array QueryParameter -> ExceptT QueryError m (Array a)
query container query parameters = parseQueryResults decodeClassy $ runEffectFn3 queryImpl container query parameters

foreign import readAllImpl :: ∀ a. EffectFn1 (Container a) (Promise (Array Json))
readAll :: ∀ a m. MonadAff m => DecodeJson a => Container a -> ExceptT QueryError m (Array a)
readAll container = parseQueryResults decodeClassy $ runEffectFn1 readAllImpl container

readAllWith :: ∀ a m. MonadAff m => JsonCodec a -> Container a -> ExceptT QueryError m (Array a)
readAllWith codec container = parseQueryResults (decodeCodec codec) $ runEffectFn1 readAllImpl container

decodeClassy :: ∀ a. DecodeJson a => Json -> Either QueryError a
decodeClassy = decodeJson >>> lmap JsonError

decodeCodec :: ∀ a. JsonCodec a -> Json -> Either QueryError a
decodeCodec codec = Codec.decode codec >>> lmap (JsonError <<< codecErrToClassyErr)

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
  promise <- try rawResult # liftEffect # ExceptT # withExceptT DBError
  results <- toAff promise # try # liftAff # ExceptT # withExceptT DBError

  traverse decode results # except

foreign import insertImpl :: ∀ a. EffectFn2 (Container a) Json (Promise Unit)
insert :: ∀ a m. MonadAff m => EncodeJson a => Container a -> a -> ExceptT String m Unit
insert container item = do
  promise <- runEffectFn2 insertImpl container (encodeJson item) # try # liftEffect # ExceptT # withExceptT message
  toAff promise # try # liftAff # ExceptT # withExceptT message

foreign import deleteImpl :: ∀ a. EffectFn3 (Container a) Json Json (Promise Unit) 
delete :: ∀ a id key m. MonadAff m => EncodeJson id => EncodeJson key => Container a -> id -> key -> ExceptT String m Unit
delete container itemID itemPartitionKey = do
  promise <- 
    runEffectFn3 deleteImpl container (encodeJson itemID) (encodeJson itemPartitionKey) 
      # try # liftEffect # ExceptT # withExceptT message
  toAff promise # try # liftAff # ExceptT # withExceptT message

deleteWith :: ∀ a id key m. MonadAff m => JsonCodec id -> JsonCodec key -> Container a -> id -> key -> ExceptT String m Unit
deleteWith idCodec keyCodec container itemID itemPartitionKey = do
  promise <- 
    runEffectFn3 deleteImpl container (Codec.encode idCodec itemID) (Codec.encode keyCodec itemPartitionKey) 
      # try # liftEffect # ExceptT # withExceptT message
  toAff promise # try # liftAff # ExceptT # withExceptT message
