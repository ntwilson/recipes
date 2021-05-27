module Recipes.Backend.DB 
  ( withConnection
  , execQuery
  , execUpdate
  , recipe
  , ingredient
  , recipeIngredients
  , settings
  , appState) where

import Backend.Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Database.PostgreSQL (class FromSQLRow, Connection)
import Recipes.DataStructures (IngredientRow, RecipeIngredientsRow, RecipeRow, SettingsRow, AppStateRow)
import Recipes.ErrorHandling (liftError)
import Selda (Col, FullQuery, Table(..))
import Selda.Col (class GetCols)
import Selda.PG.Aff (query, update)
import Selda.PG.Utils (class ColsToPGHandler, class TableToColsWithoutAlias)

data Client 
foreign import newClient :: ∀ a. Record a -> Effect Client
foreign import connect :: Client -> Effect $ Promise Connection
foreign import disconnect :: Connection -> Effect $ Promise Unit


connection :: ∀ eff. MonadAff eff => eff Connection
connection = do
  mode <- env "MODE"
  if mode /= Just "development"
  then do
    connectionString <- fromMaybe "" <$> env "DATABASE_URL"
    client <- liftEffect $ newClient { connectionString, ssl: { rejectUnauthorized: false } }
    liftAff $ (join $ liftEffect $ Promise.toAff <$> connect client)
  else do
    database <- fromMaybe "recipes" <$> env "DATABASE_NAME"
    user <- fromMaybe "" <$> env "DATABASE_USER"
    password <- fromMaybe "" <$> env "DATABASE_PASSWORD"
    client <- liftEffect $ newClient { user, database, password }
    liftAff $ (join $ liftEffect $ Promise.toAff <$> connect client)
  
  where
    env = liftEffect <<< lookupEnv

withConnection :: ∀ eff a. MonadAff eff => (Connection -> eff a) -> eff a
withConnection action = do
  conn <- connection
  ans <- action conn
  liftAff $ (join $ liftEffect $ Promise.toAff <$> disconnect conn)
  pure ans

execQuery ∷ ∀ o i tup s
  . ColsToPGHandler s i tup o
  => GetCols i
  => FromSQLRow tup
  => Connection → FullQuery s (Record i) → Aff $ Array { | o }
execQuery conn qry = query conn qry >>= liftError

execUpdate
  ∷ ∀ r s r'
  . TableToColsWithoutAlias r r'
  => GetCols r'
  => Connection 
  → Table r 
  → ({ | r' } → Col s Boolean) 
  → ({ | r' } → { | r' })
  → Aff Unit
execUpdate conn table pred up = update conn table pred up >>= liftError


recipe :: Table RecipeRow
recipe = Table { name: "recipe" }

ingredient :: Table IngredientRow
ingredient = Table { name: "ingredient" }

recipeIngredients :: Table RecipeIngredientsRow
recipeIngredients = Table { name: "recipeIngredients" }

settings :: Table SettingsRow
settings = Table { name: "settings" }

appState :: Table AppStateRow
appState = Table { name: "appState" }


