module Recipes.Backend.DB where

import Backend.Prelude

import Control.Monad.Except (ExceptT)
import Control.Promise (Promise, toAff)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

type ConnectConfig = 
  { endpoint :: String
  , key :: String
  , databaseId :: String
  }

newtype PartitionKeyDefinition = PartitionKeyDefinition { paths :: Array String }
-- partition keys must have just a single path
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
  lift $ liftEffect $ runEffectFn1 cosmosClient config

newConnection :: ∀ m. MonadEffect m => ExceptT String m Database
newConnection = do
  config <- connectionConfig
  lift $ liftEffect $ runEffectFn1 database config

foreign import data Database :: Type
foreign import data Container :: Type -> Type
foreign import data CosmosClient :: Type
foreign import cosmosClient :: ∀ r. EffectFn1 { endpoint :: String, key :: String | r } CosmosClient
foreign import database :: EffectFn1 ConnectConfig Database

class HasDbRepresentation :: Type -> Constraint
class HasDbRepresentation a

foreign import queryImpl :: ∀ a. Container a -> String -> Effect (Promise (Array a))

query :: ∀ a. HasDbRepresentation a => Container a -> String -> Aff (Array a)
query container query = do
  promise <- liftEffect $ queryImpl container query
  toAff promise

-- foreign import newClient :: ∀ a. Record a -> Effect ConnectReady
-- foreign import connect :: ConnectReady -> Effect $ Promise Client
-- foreign import disconnect :: Client -> Effect $ Promise Unit
-- foreign import unsafeStringify :: ∀ a. a -> String

-- type Connection = Client

-- client :: ∀ aff. MonadAff aff => aff ConnectReady
-- client = do
--   mode <- env "MODE"
--   if mode /= Just "development"
--   then do
--     connectionString <- fromMaybe "" <$> env "DATABASE_URL"
--     liftEffect $ newClient { connectionString, ssl: { rejectUnauthorized: false } }
--   else do
--     database <- fromMaybe "recipes" <$> env "DATABASE_NAME"
--     user <- fromMaybe "" <$> env "DATABASE_USER"
--     password <- fromMaybe "" <$> env "DATABASE_PASSWORD"
--     liftEffect $ newClient { user, database, password }
  
--   where
--     env = liftEffect <<< lookupEnv


-- connection :: ∀ aff. MonadAff aff => aff Connection 
-- connection = do
--   cl <- client
--   liftAff $ (join $ liftEffect $ Promise.toAff <$> connect cl)

-- withConnection :: ∀ aff a. MonadAff aff => (Connection -> aff a) -> aff a
-- withConnection action = do
--   conn <- connection
--   ans <- action conn
--   liftAff $ (join $ liftEffect $ Promise.toAff <$> disconnect conn)
--   pure ans

-- decodeWithError :: ∀ a. Decode a => Foreign -> Either Error a
-- decodeWithError f = lmap (error <<< renderManyErrors) $ unwrap $ runExceptT $ decode f
--   where 
--   renderManyErrors = intercalate ";\n" <<< map renderForeignError

-- execQuery :: ∀ a. Decode a => Client -> Query a -> Aff $ Array a
-- execQuery conn qry@(Query qryStr) = do
--   log $ i"Executing> "qryStr
--   query_ decodeWithError qry conn

-- execUpdate :: ∀ s. Client -> Query s -> Array SqlValue -> Aff Unit
-- execUpdate conn query@(Query qryStr) vals = do
--   log $ i"Executing> "qryStr
--   log $ i"  with values> "(show (unsafeStringify <$> vals))
--   execute query vals conn

-- recipeTable :: String
-- recipeTable = "recipe"

-- ingredientTable :: String
-- ingredientTable = "ingredient"

-- recipeIngredientsTable :: String
-- recipeIngredientsTable = "recipeIngredients"

-- settingsTable :: String
-- settingsTable = "settings"

-- appStateTable :: String
-- appStateTable = "appState"

-- recipeStepsTable :: String
-- recipeStepsTable = "recipeSteps"
