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
foreign import newClient :: forall a. Record a -> Effect Client
foreign import connect :: Client -> Effect Connection

connection :: Effect Connection
connection = do
  mode <- lookupEnv "MODE"
  if mode /= Just "development"
  then do
    connectionString <- fromMaybe "" <$> lookupEnv "DATABASE_URL"
    client <- newClient { connectionString, ssl: { rejectUnauthorized: false } }
    connect client
  else do
    database <- fromMaybe "recipes" <$> lookupEnv "DATABASE_NAME"
    user <- fromMaybe "" <$> lookupEnv "DATABASE_USER"
    password <- fromMaybe "" <$> lookupEnv "DATABASE_PASSWORD"
    client <- newClient { user, database, password }
    connect client

execQuery ∷ ∀ o i tup s
  . ColsToPGHandler s i tup o
  ⇒ GetCols i
  ⇒ FromSQLRow tup
  ⇒ Connection → FullQuery s (Record i) → Aff $ Array { | o }
execQuery conn qry = do
  result <- query conn qry
  liftError result

recipe :: Table Recipe
recipe = Table { name: "recipe" }

ingredient :: Table Ingredient
ingredient = Table { name: "ingredient" }

recipeIngredients :: Table RecipeIngredients
recipeIngredients = Table { name: "recipeIngredients" }

settings :: Table Settings
settings = Table { name: "settings" }

