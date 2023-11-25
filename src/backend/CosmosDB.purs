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
  , jsonDecodeError
  , DBERROR
  , stringError
  , dbError
  , QUERY_ERROR
  , NO_MATCH_ERROR
  , noMatchError
  , DELETE_ERROR
  , STRING_ERROR
  )
  where

import Backend.Prelude

import Control.Monad.Except (except)
import Control.Promise (Promise, toAff)
import Data.Array as Array
import Data.Newtype (class Newtype, wrap)
import Data.Variant (Variant, inj)
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

type STRING_ERROR r = (stringError :: String | r)
stringError :: ∀ r. String -> Variant (STRING_ERROR + r)
stringError = inj (Proxy :: _ "stringError")

type ERROR r = (error :: Error | r)
err :: ∀ r. Error -> Variant (ERROR + r)
err = inj (Proxy :: _ "error")

connectionConfig :: ∀ r m. MonadEffect m => ExceptV (STRING_ERROR + r) m ConnectConfig
connectionConfig = do
  key <- env "COSMOS_KEY"
  databaseId <- env "COSMOS_DB"

  pure $ 
    { endpoint: "https://ntw-cosmos.documents.azure.com:443/"
    , key 
    , databaseId
    }

  where 
  env :: String -> ExceptV (STRING_ERROR + r) m String
  env keyname =
    liftEffect (lookupEnv keyname) >>= case _ of
      Nothing -> throwError $ stringError $ i"No "keyname" environment variable found"
      Just key -> pure key

catchEffect :: ∀ r a m. MonadEffect m => Effect a -> ExceptV (STRING_ERROR + r) m a
catchEffect eff = try eff <#> lmap (stringError <<< message) # liftEffect # wrap

newClient :: ∀ r m. MonadEffect m => ExceptV (STRING_ERROR + r) m CosmosClient
newClient = do
  config <- connectionConfig
  runEffectFn1 cosmosClient config # catchEffect

newConnection :: ∀ r m. MonadEffect m => ExceptV (STRING_ERROR + r) m Database
newConnection = do
  config <- connectionConfig
  runEffectFn1 database config # catchEffect

foreign import data Database :: Type
foreign import data RawContainer :: Type
foreign import data CosmosClient :: Type
foreign import cosmosClient :: ∀ r. EffectFn1 { endpoint :: String, key :: String | r } CosmosClient
foreign import database :: EffectFn1 ConnectConfig Database
foreign import getContainerImpl :: EffectFn2 Database String (RawContainer)

getContainer :: ∀ c a r m. Container c a => MonadEffect m => ExceptV (STRING_ERROR + r) m c
getContainer = do
  db <- newConnection
  raw <- runEffectFn2 getContainerImpl db (containerName (Proxy :: _ c)) # catchEffect
  pure $ wrap raw

type DBERROR r = (dbError :: Error | r)
dbError :: ∀ r. Error -> Variant (DBERROR + r)
dbError = inj (Proxy :: _ "dbError")

printDBError :: ∀ r a m. Monad m => ExceptV (DBERROR + STRING_ERROR + r) m a -> ExceptV (STRING_ERROR + r) m a
printDBError = handleError { dbError: throwError <<< stringError <<< message }

type JSON_DECODE_ERROR r = (jsonDecodeError :: JsonDecodeError | r)
jsonDecodeError :: ∀ r. JsonDecodeError -> Variant (JSON_DECODE_ERROR + r)
jsonDecodeError = inj (Proxy :: _ "jsonDecodeError")

type QUERY_ERROR r = DBERROR + JSON_DECODE_ERROR + r

printQueryError :: ∀ r a m. Monad m => ExceptV (QUERY_ERROR + STRING_ERROR + r) m a -> ExceptV (STRING_ERROR + r) m a
printQueryError = printDBError <<< handleError { jsonDecodeError: throwError <<< stringError <<< printJsonDecodeError }

type QueryParameter = { name :: String, value :: Json }
foreign import queryImpl :: EffectFn3 RawContainer String (Array QueryParameter) (Promise (Array Json))
query :: ∀ c a b r m. Container c a => MonadAff m => JsonCodec b -> c -> String -> Array QueryParameter -> ExceptV (QUERY_ERROR + r) m (Array b)
query codec container query parameters = parseQueryResults codec $ runEffectFn3 queryImpl (unwrap container) query parameters

foreign import readAllImpl :: EffectFn1 RawContainer (Promise (Array Json))

readAll :: ∀ c r a m. Container c a => MonadAff m => JsonCodec a -> c -> ExceptV (QUERY_ERROR + r) m (Array a)
readAll codec container = parseQueryResults codec $ runEffectFn1 readAllImpl $ unwrap container

parseQueryResults :: ∀ r a m. MonadAff m => JsonCodec a -> Effect (Promise (Array Json)) -> ExceptV (QUERY_ERROR + r) m (Array a)
parseQueryResults codec rawResult = do
  results <- effPromiseToAff rawResult # handleError { error: throwError <<< dbError }
  traverse (decode codec) results # lmap jsonDecodeError # except

foreign import insertImpl :: EffectFn2 RawContainer Json (Promise Unit)
insert :: ∀ c r a m. 
  Container c a => MonadAff m => JsonCodec a -> c -> a -> ExceptV (STRING_ERROR + r) m Unit
insert codec container item = do
  runEffectFn2 insertImpl (unwrap container) (encode codec item) 
    # effPromiseToAff 
    # handleError { error: throwError <<< stringError <<< message }

newtype ItemID = ItemID String
derive instance Newtype ItemID _

-- all records in a cosmos database come with an id field.
getID :: Json -> ItemID
getID json = (unsafeCoerce json).id

foreign import deleteImpl :: EffectFn3 RawContainer ItemID String (Promise Unit) 

pointDelete :: ∀ c r a m. Container c a => MonadAff m => c -> ItemID -> String -> ExceptV (ERROR + r) m Unit
pointDelete container itemID key = 
  runEffectFn3 deleteImpl (unwrap container) itemID key # effPromiseToAff


type NO_MATCH_ERROR r = (noMatchError :: String | r)
noMatchError :: ∀ r. String -> Variant (NO_MATCH_ERROR r)
noMatchError collectionName = inj (Proxy :: _ "noMatchError") collectionName

printNoMatchError :: ∀ r a m. Monad m => ExceptV (NO_MATCH_ERROR + STRING_ERROR + r) m a -> ExceptV (STRING_ERROR + r) m a
printNoMatchError = handleError { noMatchError: \collectionName -> throwError $ stringError $ i"No matching "collectionName" record was found in the database to delete" }

type DELETE_ERROR r = QUERY_ERROR + NO_MATCH_ERROR + r
printDeleteError :: ∀ r a m. Monad m => ExceptV (DELETE_ERROR + STRING_ERROR + r) m a -> ExceptV (STRING_ERROR + r) m a
printDeleteError = printQueryError <<< printNoMatchError

deleteViaFind :: ∀ c r a m. Container c a => MonadAff m =>
  JsonCodec a -> (a -> a -> Boolean) -> c -> a -> ExceptV (DELETE_ERROR + r) m Unit
deleteViaFind originalCodec equate container item = do

  items <- runEffectFn1 readAllImpl (unwrap container) # parseQueryResults codec
  target <- 
    items 
    # Array.find (\{decoded} -> equate item decoded)
    # note (noMatchError $ containerName $ Proxy @c)
    # except

  let
    itemID = getID target.json
    itemPartitionKey = getPartitionKey (partitionKey :: PartitionKey c a) item

  runEffectFn3 deleteImpl (unwrap container) itemID itemPartitionKey # effPromiseToAff # handleError { error: throwError <<< dbError }

  where
  codec = codec' decoder encoder
  decoder json = do
    decoded <- decode originalCodec json
    pure {json, decoded}
  encoder {json} = json

effPromiseToAff :: ∀ r a m. MonadAff m => Effect (Promise a) -> ExceptV (ERROR + r) m a
effPromiseToAff eff = do
  promise <- eff # try <#> lmap err # liftEffect # wrap
  toAff promise # try <#> lmap err # liftAff # wrap

foreign import getItemImpl :: EffectFn5 (Json -> Maybe Json) (Maybe Json) RawContainer ItemID String (Promise (Maybe Json))

getItem :: ∀ c r a m. Container c a => MonadAff m => JsonCodec a -> c -> ItemID -> String -> ExceptV (QUERY_ERROR + r) m (Maybe a)
getItem codec container id key = do
  maybeJson <- runEffectFn5 getItemImpl Just Nothing (unwrap container) id key # effPromiseToAff # handleError { error: throwError <<< dbError }

  case maybeJson of 
    Nothing -> pure Nothing
    Just json -> decode codec json # lmap jsonDecodeError # except <#> Just

