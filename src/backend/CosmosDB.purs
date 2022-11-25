module Recipes.Backend.CosmosDB
  ( Container
  , CosmosClient
  , Database
  , DeleteError(..)
  , ItemID(..)
  , PartitionKey(..)
  , PartitionKeyDefinition
  , QueryError(..)
  , QueryParameter
  , deleteViaFind
  , getContainer
  , getItem
  , getPartitionKey
  , insert
  , newClient
  , newConnection
  , newPartitionKeyDef
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

getPartitionKey :: ∀ a. PartitionKey a -> a -> String
getPartitionKey (PartitionKey {accessor}) = accessor

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

getContainer :: ∀ a m. MonadEffect m => String -> PartitionKey a -> ExceptT String m (Container a)
getContainer containerName partitionKey = do
  database <- newConnection
  raw <- runEffectFn2 getContainerImpl database containerName # catchEffect
  pure $ Container { raw, partitionKey }

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

readAll :: ∀ a m. MonadAff m => JsonCodec a -> Container a -> ExceptT QueryError m (Array a)
readAll codec (Container {raw}) = parseQueryResults codec $ runEffectFn1 readAllImpl raw

parseQueryResults :: ∀ m a. MonadAff m => JsonCodec a -> Effect (Promise (Array Json)) -> ExceptT QueryError m (Array a)
parseQueryResults codec rawResult = do
  results <- effPromiseToAff rawResult # withExceptT DBError
  traverse (decode codec >>> lmap JsonError) results # except


foreign import insertImpl :: EffectFn2 RawContainer Json (Promise Unit)
insert :: ∀ a m. MonadAff m => JsonCodec a -> Container a -> a -> ExceptT String m Unit
insert codec (Container {raw}) item = do
  runEffectFn2 insertImpl raw (encode codec item) # effPromiseToAff # withExceptT message

newtype ItemID = ItemID String

-- all records in a cosmos database come with an id field.
getID :: Json -> ItemID
getID json = (unsafeCoerce json).id

foreign import deleteImpl :: EffectFn3 RawContainer ItemID String (Promise Unit) 

pointDelete :: ∀ a m. MonadAff m => Container a -> ItemID -> String -> ExceptT Error m Unit
pointDelete (Container {raw}) itemID key = 
  runEffectFn3 deleteImpl raw itemID key # effPromiseToAff

data DeleteError = NoMatchFound | Err QueryError
printDeleteError :: String -> DeleteError -> String 
printDeleteError collectionName NoMatchFound = i"No matching "collectionName" record was found in the database to delete"
printDeleteError _ (Err err) = printQueryError err

deleteViaFind :: ∀ a m. MonadAff m => 
  JsonCodec a -> (a -> a -> Boolean) -> Container a -> a -> ExceptT DeleteError m Unit
deleteViaFind originalCodec equate (Container {raw, partitionKey: PartitionKey { accessor }}) item = do

  items <- runEffectFn1 readAllImpl raw # parseQueryResults codec # withExceptT Err
  target <- 
    items 
    # Array.find (\{decoded} -> equate item decoded)
    # note NoMatchFound
    # except

  let
    itemID = getID target.json
    itemPartitionKey = accessor item

  runEffectFn3 deleteImpl raw itemID itemPartitionKey # effPromiseToAff # withExceptT (Err <<< DBError)

  where
  codec = basicCodec decoder encoder
  decoder json = do
    decoded <- decode originalCodec json
    pure {json, decoded}
  encoder {json} = json

effPromiseToAff :: ∀ m a. MonadAff m => Effect (Promise a) -> ExceptT Error m a
effPromiseToAff eff = do
  promise <- eff # try # liftEffect # ExceptT
  toAff promise # try # liftAff # ExceptT

foreign import getItemImpl :: EffectFn3 RawContainer ItemID String (Promise Json)

getItem :: ∀ a m. MonadAff m => JsonCodec a -> Container a -> ItemID -> String -> ExceptT QueryError m a
getItem codec (Container {raw}) id key = do
  json <- runEffectFn3 getItemImpl raw id key # effPromiseToAff # withExceptT DBError
  decode codec json # lmap JsonError # except
