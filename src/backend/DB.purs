module Recipes.Backend.DB where

import Backend.Prelude

import Database.PostgreSQL (class FromSQLRow, Connection)
import Recipes.DataStructures (Ingredient, Recipe, RecipeIngredients, Settings)
import Recipes.ErrorHandling (liftError)
import Selda (FullQuery, Table(..))
import Selda.Col (class GetCols)
import Selda.PG.Aff (query)
import Selda.PG.Utils (class ColsToPGHandler)

data Client 
foreign import newClient :: ∀ a. Record a -> Effect Client
foreign import connect :: Client -> Effect Connection

connection :: ∀ eff. MonadEffect eff => eff Connection
connection = do
  mode <- env "MODE"
  if mode /= Just "development"
  then do
    connectionString <- fromMaybe "" <$> env "DATABASE_URL"
    client <- liftEffect $ newClient { connectionString, ssl: { rejectUnauthorized: false } }
    liftEffect $ connect client
  else do
    database <- fromMaybe "recipes" <$> env "DATABASE_NAME"
    user <- fromMaybe "" <$> env "DATABASE_USER"
    password <- fromMaybe "" <$> env "DATABASE_PASSWORD"
    client <- liftEffect $ newClient { user, database, password }
    liftEffect $ connect client
  
  where
    env = liftEffect <<< lookupEnv

execQuery ∷ ∀ o i tup s
  . ColsToPGHandler s i tup o
  => GetCols i
  => FromSQLRow tup
  => Connection → FullQuery s (Record i) → Aff $ Array { | o }
execQuery conn qry = query conn qry >>= liftError

recipe :: Table Recipe
recipe = Table { name: "recipe" }

ingredient :: Table Ingredient
ingredient = Table { name: "ingredient" }

recipeIngredients :: Table RecipeIngredients
recipeIngredients = Table { name: "recipeIngredients" }

settings :: Table Settings
settings = Table { name: "settings" }

