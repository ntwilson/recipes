module Recipes.Backend.DB where

import Backend.Prelude

import Control.Monad.Except (runExceptT)
import Control.Promise (Promise)
import Control.Promise as Promise
-- import Database.Postgres (Client, Query(..), execute, query_)
-- import Database.Postgres.SqlValue (SqlValue)
import Foreign (renderForeignError)
-- import Foreign.Generic (class Decode, Foreign, decode)

data ConnectReady
foreign import newClient :: ∀ a. Record a -> Effect ConnectReady
foreign import connect :: ConnectReady -> Effect $ Promise Client
foreign import disconnect :: Client -> Effect $ Promise Unit
foreign import unsafeStringify :: ∀ a. a -> String

type Connection = Client

client :: ∀ aff. MonadAff aff => aff ConnectReady
client = do
  mode <- env "MODE"
  if mode /= Just "development"
  then do
    connectionString <- fromMaybe "" <$> env "DATABASE_URL"
    liftEffect $ newClient { connectionString, ssl: { rejectUnauthorized: false } }
  else do
    database <- fromMaybe "recipes" <$> env "DATABASE_NAME"
    user <- fromMaybe "" <$> env "DATABASE_USER"
    password <- fromMaybe "" <$> env "DATABASE_PASSWORD"
    liftEffect $ newClient { user, database, password }
  
  where
    env = liftEffect <<< lookupEnv


connection :: ∀ aff. MonadAff aff => aff Connection 
connection = do
  cl <- client
  liftAff $ (join $ liftEffect $ Promise.toAff <$> connect cl)

withConnection :: ∀ aff a. MonadAff aff => (Connection -> aff a) -> aff a
withConnection action = do
  conn <- connection
  ans <- action conn
  liftAff $ (join $ liftEffect $ Promise.toAff <$> disconnect conn)
  pure ans

decodeWithError :: ∀ a. Decode a => Foreign -> Either Error a
decodeWithError f = lmap (error <<< renderManyErrors) $ unwrap $ runExceptT $ decode f
  where 
  renderManyErrors = intercalate ";\n" <<< map renderForeignError

execQuery :: ∀ a. Decode a => Client -> Query a -> Aff $ Array a
execQuery conn qry@(Query qryStr) = do
  log $ i"Executing> "qryStr
  query_ decodeWithError qry conn

execUpdate :: ∀ s. Client -> Query s -> Array SqlValue -> Aff Unit
execUpdate conn query@(Query qryStr) vals = do
  log $ i"Executing> "qryStr
  log $ i"  with values> "(show (unsafeStringify <$> vals))
  execute query vals conn

recipeTable :: String
recipeTable = "recipe"

ingredientTable :: String
ingredientTable = "ingredient"

recipeIngredientsTable :: String
recipeIngredientsTable = "recipeIngredients"

settingsTable :: String
settingsTable = "settings"

appStateTable :: String
appStateTable = "appState"

recipeStepsTable :: String
recipeStepsTable = "recipeSteps"
