module Recipes.Backend.DB where

import Backend.Prelude

import Database.PostgreSQL (Connection)
import Selda (Table(..))

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

type Recipe =
  ( name :: String
  , fullDescription :: String
  )
recipe :: Table Recipe
recipe = Table { name: "recipe" }

type Ingredient =
  ( name :: String
  , store :: String
  , section :: Maybe String
  , common :: Boolean
  )
ingredient :: Table Ingredient
ingredient = Table { name: "ingredient" }

type RecipeIngredients =
  ( recipe :: String
  , ingredient :: String
  , quantity :: Number
  , units :: Maybe String
  )
recipeIngredients :: Table RecipeIngredients
recipeIngredients = Table { name: "recipeIngredients" }

type Settings = 
  ( name :: String
  , value :: String
  )
settings :: Table Settings
settings = Table { name: "settings" }

