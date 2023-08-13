module Recipes.Backend.CosmosDB
  ( CosmosClient
  , Database
  , ItemID(..)
  , PartitionKey(..)
  , PartitionKeyDefinition
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
  , printDBError
  , printDeleteError
  , printQueryError
  , query
  , readAll
  , JSON_DECODE_ERROR
  , _jsonDecodeError
  , DBERROR
  , _dbError
  , QUERY_ERROR
  , NO_MATCH_ERROR
  , _noMatchError
  , DELETE_ERROR
  )
  where

import Backend.Prelude

import Control.Promise (Promise, toAff)
import Data.Array as Array
import Data.Newtype (class Newtype, wrap)
import Effect.Uncurried (EffectFn5, runEffectFn5)
import Run.Except (_except, fromJustAt)
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

connectionConfig :: ∀ r. Run (EFFECT + EXCEPT String + r) ConnectConfig
connectionConfig = do
  key <- env "COSMOS_KEY"
  databaseId <- env "COSMOS_DB"

  pure $ 
    { endpoint: "https://ntw-cosmos.documents.azure.com:443/"
    , key 
    , databaseId
    }

  where 
  env :: String -> Run (EFFECT + EXCEPT String + r) String
  env keyname = 
    liftEffect (lookupEnv keyname) >>= case _ of
      Nothing -> throw $ i"No "keyname" environment variable found"
      Just key -> pure key

catchEffect :: ∀ r a. Effect a -> Run (EFFECT + EXCEPT String + r) a
catchEffect eff = try eff <#> lmap message # liftEffect >>= rethrow

newClient :: ∀ r. Run (EFFECT + EXCEPT String + r) CosmosClient
newClient = do
  config <- connectionConfig
  runEffectFn1 cosmosClient config # catchEffect

newConnection :: ∀ r. Run (EFFECT + EXCEPT String + r) Database
newConnection = do
  config <- connectionConfig
  runEffectFn1 database config # catchEffect

foreign import data Database :: Type
foreign import data RawContainer :: Type
foreign import data CosmosClient :: Type
foreign import cosmosClient :: ∀ r. EffectFn1 { endpoint :: String, key :: String | r } CosmosClient
foreign import database :: EffectFn1 ConnectConfig Database
foreign import getContainerImpl :: EffectFn2 Database String (RawContainer)

getContainer :: ∀ c a r. Container c a => Run (EFFECT + EXCEPT String + r) c
getContainer = do
  db <- newConnection
  raw <- runEffectFn2 getContainerImpl db (containerName (Proxy :: _ c)) # catchEffect
  pure $ wrap raw

type DBERROR :: ∀ k. Row (k -> Type) -> Row (k -> Type)
type DBERROR r = (dbError :: Except Error | r)
_dbError :: Proxy "dbError"
_dbError = Proxy

printDBError :: ∀ r a. Run (DBERROR + EXCEPT String + r) a -> Run (EXCEPT String + r) a
printDBError = withExceptAt _dbError _except message

type JSON_DECODE_ERROR :: ∀ k. Row (k -> Type) -> Row (k -> Type)
type JSON_DECODE_ERROR r = (jsonDecodeError :: Except JsonDecodeError | r)
_jsonDecodeError :: Proxy "jsonDecodeError"
_jsonDecodeError = Proxy

type QUERY_ERROR :: ∀ k. Row (k -> Type) -> Row (k -> Type)
type QUERY_ERROR r = DBERROR + JSON_DECODE_ERROR + r

printQueryError :: ∀ r a. Run (QUERY_ERROR + EXCEPT String + r) a -> Run (EXCEPT String + r) a
printQueryError = printDBError <<< withExceptAt _jsonDecodeError _except printJsonDecodeError

type QueryParameter = { name :: String, value :: Json }
foreign import queryImpl :: EffectFn3 RawContainer String (Array QueryParameter) (Promise (Array Json))
query :: ∀ c a b r. Container c a => JsonCodec b -> c -> String -> Array QueryParameter -> Run (AFFECT + QUERY_ERROR + r) (Array b)
query codec container query parameters = parseQueryResults codec $ runEffectFn3 queryImpl (unwrap container) query parameters

foreign import readAllImpl :: EffectFn1 RawContainer (Promise (Array Json))

readAll :: ∀ c a r. Container c a => JsonCodec a -> c -> Run (AFFECT + QUERY_ERROR + r) (Array a)
readAll codec container = parseQueryResults codec $ runEffectFn1 readAllImpl $ unwrap container

parseQueryResults :: ∀ r a. JsonCodec a -> Effect (Promise (Array Json)) -> Run(AFFECT + QUERY_ERROR + r) (Array a)
parseQueryResults codec rawResult = do
  results <- effPromiseToAff rawResult # moveExcept _except _dbError
  traverse (decode codec) results # rethrowAt _jsonDecodeError

foreign import insertImpl :: EffectFn2 RawContainer Json (Promise Unit)
insert :: ∀ c a r. 
  Container c a => JsonCodec a -> c -> a -> Run (AFFECT + EXCEPT String + r) Unit
insert codec container item = do
  runEffectFn2 insertImpl (unwrap container) (encode codec item) # effPromiseToAff # withExcept message

newtype ItemID = ItemID String
derive instance Newtype ItemID _

-- all records in a cosmos database come with an id field.
getID :: Json -> ItemID
getID json = (unsafeCoerce json).id

foreign import deleteImpl :: EffectFn3 RawContainer ItemID String (Promise Unit) 

pointDelete :: ∀ c a r. Container c a => c -> ItemID -> String -> Run (AFFECT + EXCEPT Error + r) Unit
pointDelete container itemID key = 
  runEffectFn3 deleteImpl (unwrap container) itemID key # effPromiseToAff


type NO_MATCH_ERROR :: ∀ k. Row (k -> Type) -> Row (k -> Type)
type NO_MATCH_ERROR r = (noMatchError :: Except Unit | r)
_noMatchError :: Proxy "noMatchError"
_noMatchError = Proxy

printNoMatchError :: ∀ r a. String -> Run (NO_MATCH_ERROR + EXCEPT String + r) a -> Run (EXCEPT String + r) a
printNoMatchError collectionName = withExceptAt _noMatchError _except (const (i"No matching "collectionName" record was found in the database to delete"))

type DELETE_ERROR :: ∀ k. Row (k -> Type) -> Row (k -> Type)
type DELETE_ERROR r = QUERY_ERROR + NO_MATCH_ERROR + r
printDeleteError :: ∀ r a. String -> Run (DELETE_ERROR + EXCEPT String + r) a -> Run (EXCEPT String + r) a
printDeleteError collectionName = printQueryError <<< printNoMatchError collectionName

deleteViaFind :: ∀ c a r. Container c a =>
  JsonCodec a -> (a -> a -> Boolean) -> c -> a -> Run (AFFECT + DELETE_ERROR + r) Unit
deleteViaFind originalCodec equate container item = do

  items <- runEffectFn1 readAllImpl (unwrap container) # parseQueryResults codec
  target <- 
    items 
    # Array.find (\{decoded} -> equate item decoded)
    # fromJustAt _noMatchError

  let
    itemID = getID target.json
    itemPartitionKey = getPartitionKey (partitionKey :: PartitionKey c a) item

  runEffectFn3 deleteImpl (unwrap container) itemID itemPartitionKey # effPromiseToAff # moveExcept _except _dbError

  where
  codec = codec' decoder encoder
  decoder json = do
    decoded <- decode originalCodec json
    pure {json, decoded}
  encoder {json} = json

effPromiseToAff :: ∀ r a. Effect (Promise a) -> Run (AFFECT + EXCEPT Error + r) a
effPromiseToAff eff = do
  promise <- eff # try # liftEffect >>= rethrow 
  toAff promise # try # liftAff >>= rethrow

foreign import getItemImpl :: EffectFn5 (Json -> Maybe Json) (Maybe Json) RawContainer ItemID String (Promise (Maybe Json))

getItem :: ∀ c a r. Container c a => JsonCodec a -> c -> ItemID -> String -> Run (AFFECT + QUERY_ERROR + r) (Maybe a)
getItem codec container id key = do
  maybeJson <- runEffectFn5 getItemImpl Just Nothing (unwrap container) id key # effPromiseToAff # moveExcept _except _dbError

  case maybeJson of 
    Nothing -> pure Nothing
    Just json -> decode codec json # rethrowAt _jsonDecodeError <#> Just
