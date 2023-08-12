module Recipes.Backend.CosmosDB
  ( CosmosClient
  , Database
  , DeleteError(..)
  , ItemID(..)
  , PartitionKey(..)
  , PartitionKeyDefinition
  , QueryError(..)
  , QueryParameter
  , RawContainer
  , class Container
  , containerName
  , deleteViaFind
  , getContainer
  , getItem
  , getPartitionKey
  , insert
  , newClient
  , newConnection
  , newPartitionKeyDef
  , partitionKey
  , pointDelete
  , printDeleteError
  , printQueryError
  , query
  , readAll
  )
  where

import Backend.Prelude

import Control.Promise (Promise, toAff)
import Data.Array as Array
import Data.Newtype (class Newtype, wrap)
import Effect.Uncurried (EffectFn5, runEffectFn5)
import Type.Proxy (Proxy(..))

-- | A `Container` `c` is just a RawContainer containing elements of type `a` with a `PartitionKey` and a name defined.
-- | Each `Container` must have exactly one name and one partition key, and this class ensures that you can't accidentally use 
-- | multiple or the wrong ones.
class Newtype c RawContainer <= Container c a | c -> a where
  partitionKey :: PartitionKey c a
  containerName :: Proxy c -> String

newtype PartitionKey :: Type -> Type -> Type
newtype PartitionKey container a = PartitionKey { def :: PartitionKeyDefinition, accessor :: a -> String }
newtype PartitionKeyDefinition = PartitionKeyDefinition { paths :: Array String }
-- partition keys must have just a single path
-- https://docs.microsoft.com/en-us/javascript/api/@azure/cosmos/partitionkeydefinition?view=azure-node-latest
newPartitionKeyDef :: String -> PartitionKeyDefinition
newPartitionKeyDef path = PartitionKeyDefinition { paths: [path] }

getPartitionKey :: ∀ c a. PartitionKey c a -> a -> String
getPartitionKey (PartitionKey {accessor}) = accessor

type ConnectConfig = 
  { endpoint :: String
  , key :: String
  , databaseId :: String
  }

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

getContainer :: ∀ c a m. Container c a => MonadEffect m => ExceptT String m c
getContainer = do
  database <- newConnection
  raw <- runEffectFn2 getContainerImpl database (containerName (Proxy :: _ c)) # catchEffect
  pure $ wrap raw

data QueryError = DBError Error | JsonError JsonDecodeError
printQueryError :: QueryError -> String
printQueryError (DBError err) = message err
printQueryError (JsonError err) = printJsonDecodeError err

type QueryParameter = { name :: String, value :: Json }
foreign import queryImpl :: EffectFn3 RawContainer String (Array QueryParameter) (Promise (Array Json))
query :: ∀ c a b m. Container c a => MonadAff m => JsonCodec b -> c -> String -> Array QueryParameter -> ExceptT QueryError m (Array b)
query codec container query parameters = parseQueryResults codec $ runEffectFn3 queryImpl (unwrap container) query parameters

foreign import readAllImpl :: EffectFn1 RawContainer (Promise (Array Json))

readAll :: ∀ c a m. Container c a => MonadAff m => JsonCodec a -> c -> ExceptT QueryError m (Array a)
readAll codec container = parseQueryResults codec $ runEffectFn1 readAllImpl $ unwrap container

parseQueryResults :: ∀ m a. MonadAff m => JsonCodec a -> Effect (Promise (Array Json)) -> ExceptT QueryError m (Array a)
parseQueryResults codec rawResult = do
  results <- effPromiseToAff rawResult # withExceptT DBError
  traverse (decode codec >>> lmap JsonError) results # except


foreign import insertImpl :: EffectFn2 RawContainer Json (Promise Unit)
insert :: ∀ c a m. Container c a => MonadAff m => JsonCodec a -> c -> a -> ExceptT String m Unit
insert codec container item = do
  runEffectFn2 insertImpl (unwrap container) (encode codec item) # effPromiseToAff # withExceptT message

newtype ItemID = ItemID String
derive instance Newtype ItemID _

-- all records in a cosmos database come with an id field.
getID :: Json -> ItemID
getID json = (unsafeCoerce json).id

foreign import deleteImpl :: EffectFn3 RawContainer ItemID String (Promise Unit) 

pointDelete :: ∀ c a m. Container c a => MonadAff m => c -> ItemID -> String -> ExceptT Error m Unit
pointDelete container itemID key = 
  runEffectFn3 deleteImpl (unwrap container) itemID key # effPromiseToAff

data DeleteError = NoMatchFound | Err QueryError
printDeleteError :: String -> DeleteError -> String 
printDeleteError collectionName NoMatchFound = i"No matching "collectionName" record was found in the database to delete"
printDeleteError _ (Err err) = printQueryError err

deleteViaFind :: ∀ c a m. Container c a => MonadAff m => 
  JsonCodec a -> (a -> a -> Boolean) -> c -> a -> ExceptT DeleteError m Unit
deleteViaFind originalCodec equate container item = do

  items <- runEffectFn1 readAllImpl (unwrap container) # parseQueryResults codec # withExceptT Err
  target <- 
    items 
    # Array.find (\{decoded} -> equate item decoded)
    # note NoMatchFound
    # except

  let
    itemID = getID target.json
    itemPartitionKey = getPartitionKey (partitionKey :: PartitionKey c a) item

  runEffectFn3 deleteImpl (unwrap container) itemID itemPartitionKey # effPromiseToAff # withExceptT (Err <<< DBError)

  where
  codec = codec' decoder encoder
  decoder json = do
    decoded <- decode originalCodec json
    pure {json, decoded}
  encoder {json} = json

effPromiseToAff :: ∀ m a. MonadAff m => Effect (Promise a) -> ExceptT Error m a
effPromiseToAff eff = do
  promise <- eff # try # liftEffect # ExceptT
  toAff promise # try # liftAff # ExceptT

foreign import getItemImpl :: EffectFn5 (Json -> Maybe Json) (Maybe Json) RawContainer ItemID String (Promise (Maybe Json))

getItem :: ∀ c a m. Container c a => MonadAff m => JsonCodec a -> c -> ItemID -> String -> ExceptT QueryError m (Maybe a)
getItem codec container id key = do
  maybeJson <- runEffectFn5 getItemImpl Just Nothing (unwrap container) id key # effPromiseToAff # withExceptT DBError
  case maybeJson of 
    Nothing -> pure Nothing
    Just json -> decode codec json # lmap JsonError # except <#> Just
