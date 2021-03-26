module Recipes.Backend.DB where

import Backend.Prelude

import Control.Monad.Except (lift, throwError)
import Control.Monad.Except.Checked (ExceptV)
import Data.Variant (SProxy(..), Variant, inj)
import Database.PostgreSQL (class FromSQLRow, Connection, PGError)
import Recipes.DataStructures (Ingredient, Recipe, RecipeIngredients, Settings)
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

type DBError r = (dbError :: PGError | r)
dbError :: forall r. PGError -> Variant (DBError r)
dbError = inj (SProxy :: _ "dbError")

execQuery ∷ ∀ o i tup s r
  . ColsToPGHandler s i tup o
  ⇒ GetCols i
  ⇒ FromSQLRow tup
  ⇒ Connection → FullQuery s (Record i) → ExceptV (DBError r) Aff $ Array { | o }
execQuery conn qry = do
  result <- lift $ query conn qry
  case result of
    Left err -> throwError $ dbError err
    Right ans -> pure ans

recipe :: Table Recipe
recipe = Table { name: "recipe" }

ingredient :: Table Ingredient
ingredient = Table { name: "ingredient" }

recipeIngredients :: Table RecipeIngredients
recipeIngredients = Table { name: "recipeIngredients" }

settings :: Table Settings
settings = Table { name: "settings" }

