module Recipes.Backend.DB.Setup where

import Backend.Prelude

import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Effect.Exception (message)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Recipes.Backend.DB (Container, CosmosClient, Database, PartitionKeyDefinition, connectionConfig, newClient, newConnection, newPartitionKeyDef)
import Recipes.Backend.ServerSetup (loadEnv)

main :: Effect Unit
main = launchAff_ do
  loadEnv
  runExceptT setupDatabase >>= case _ of 
    Left err -> log err
    Right _ -> pure unit

foreign import createDatabase :: EffectFn2 CosmosClient String Database
foreign import createContainer :: ∀ a. EffectFn3 Database String PartitionKeyDefinition (Container a)

setupDatabase :: ∀ m. MonadEffect m => ExceptT String m Unit
setupDatabase = do
  config <- connectionConfig
  conn <- newClient
  void $ handleFFI $ runEffectFn2 createDatabase conn config.databaseId
  log $ i"Created database "config.databaseId
  db <- newConnection
  void $ handleFFI $ runEffectFn3 createContainer db "recipe" $ newPartitionKeyDef "/name"
  log $ i"Created recipe container"

  where
  handleFFI :: ∀ a. Effect a -> ExceptT String m a
  handleFFI fn = withExceptT message $ ExceptT (liftEffect $ try fn)
