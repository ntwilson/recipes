module Recipes.Backend.DB.Setup where

import Backend.Prelude

import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Promise (Promise, toAff)
import Effect.Exception (message)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Recipes.Backend.DB (Container, CosmosClient, Database, PartitionKeyDefinition, connectionConfig, ingredientsContainer, newClient, newConnection, newPartitionKeyDef, recipeContainer)
import Recipes.Backend.DB as DB
import Recipes.Backend.ServerSetup (loadEnv)

main :: Effect Unit
main = launchAff_ do
  loadEnv
  runExceptT setupDatabase >>= case _ of 
    Left err -> log err
    Right _ -> pure unit

foreign import createDatabase :: EffectFn2 CosmosClient String (Promise Database)
foreign import createContainer :: ∀ a. EffectFn3 Database String PartitionKeyDefinition (Promise (Container a))
foreign import deleteContainer :: Database -> String -> Effect (Promise Unit)

setupDatabase :: ∀ m. MonadAff m => ExceptT String m Unit
setupDatabase = do
  setupSchema
  populateData

setupSchema :: ∀ m. MonadAff m => ExceptT String m Unit
setupSchema = do
  config <- connectionConfig
  conn <- newClient
  void $ handleFFI $ runEffectFn2 createDatabase conn config.databaseId
  log $ i"Created database "config.databaseId
  db <- newConnection
  deleteContainer db "recipes" # handleFFI # runExceptT # void # lift
  void $ handleFFI $ runEffectFn3 createContainer db "recipes" $ newPartitionKeyDef "/name"
  log "Created recipes container"
  deleteContainer db "ingredients" # handleFFI # runExceptT # void # lift
  void $ handleFFI $ runEffectFn3 createContainer db "ingredients" $ newPartitionKeyDef "/name"
  log "Created ingredients container"

  where
  handleFFI :: ∀ a. Effect (Promise a) -> ExceptT String m a
  handleFFI fn = do
    promise <- withExceptT message $ ExceptT $ liftEffect $ try fn
    withExceptT message $ ExceptT $ liftAff $ try $ toAff promise

populateData :: ∀ m. MonadAff m => ExceptT String m Unit
populateData = do
  conn <- newConnection
  populateRecipes conn
  populateIngredients conn

  where
  populateRecipes conn = do
    container <- recipeContainer conn
    DB.insert container {name: "Balsamic Tomato & Herb Chicken"}
    DB.insert container {name: "Butter Chicken"}
    DB.insert container {name: "Firecracker Meatballs"}
    DB.insert container {name: "Pork Carnitas Tacos"}
    DB.insert container {name: "Creamy White Chicken Chili"}
    DB.insert container {name: "Zuppa Toscana"}
    DB.insert container {name: "Simple Chili"}
    DB.insert container {name: "Chicken Tikka Masala"}
    DB.insert container {name: "Chicken Lettuce Wraps"}
    DB.insert container {name: "Swiss Chicken"}
    DB.insert container {name: "Split Pea Soup"}
    DB.insert container {name: "The Staples"}
    log "Populated recipes"

  populateIngredients conn = do
    container <- ingredientsContainer conn
    DB.insert container {name: "Almond flour", store: "Harris Teeter", section: Nothing, common: true}
    DB.insert container {name: "Annika's Wine", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Bacon", store: "ALDI", section: Just "Meat", common: true}
    DB.insert container {name: "Balsamic vinegar", store: "Costco", section: Nothing, common: true}
    DB.insert container {name: "Bay leaves", store: "Harris Teeter", section: Just "Produce", common: false}
    DB.insert container {name: "Brown sugar", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Butter", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Carrots", store: "ALDI", section: Just "Produce", common: true}
    DB.insert container {name: "Cauliflower rice", store: "ALDI", section: Just "Frozen", common: true}
    DB.insert container {name: "Cayenne pepper", store: "ALDI", section: Just "Spices", common: true}
    DB.insert container {name: "Celery", store: "ALDI", section: Just "Produce", common: true}
    DB.insert container {name: "Chicken", store: "Costco", section: Just "Meat", common: false}
    DB.insert container {name: "Chicken bouillon", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Chicken stock", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Chili flakes", store: "Harris Teeter", section: Just "Spices", common: true}
    DB.insert container {name: "Chili starter", store: "ALDI", section: Just "Canned goods", common: true}
    DB.insert container {name: "Chipotle powder", store: "Harris Teeter", section: Just "Spices", common: true}
    DB.insert container {name: "Chips", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Chives", store: "Harris Teeter", section: Just "Produce", common: false}
    DB.insert container {name: "Cilantro", store: "ALDI", section: Just "Produce", common: true}
    DB.insert container {name: "Coconut milk", store: "Harris Teeter", section: Just "Dairy", common: false}
    DB.insert container {name: "Colby cheese", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Coriander", store: "Harris Teeter", section: Just "Spices", common: true}
    DB.insert container {name: "Cream cheese", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Cream of chicken", store: "ALDI", section: Just "Canned goods", common: true}
    DB.insert container {name: "Cumin", store: "ALDI", section: Just "Spices", common: true}
    DB.insert container {name: "Diced tomatoes", store: "ALDI", section: Just "Canned goods", common: false}
    DB.insert container {name: "Eggs", store: "Costco", section: Just "Dairy", common: true}
    DB.insert container {name: "Frozen Corn", store: "ALDI", section: Just "Frozen", common: true}
    DB.insert container {name: "Garam masala", store: "Harris Teeter", section: Just "Spices", common: true}
    DB.insert container {name: "Garlic", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Ginger", store: "Costco", section: Nothing, common: true}
    DB.insert container {name: "Great Northern beans", store: "ALDI", section: Just "Canned goods", common: false}
    DB.insert container {name: "Greek yogurt", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Ground beef", store: "ALDI", section: Just "Meat", common: false}
    DB.insert container {name: "Ground pork", store: "ALDI", section: Just "Meat", common: false}
    DB.insert container {name: "Ham", store: "ALDI", section: Just "Meat", common: false}
    DB.insert container {name: "Half & half", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Heavy cream", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Hoisin sauce", store: "Harris Teeter", section: Nothing, common: true}
    DB.insert container {name: "Honey", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Italian sausage", store: "ALDI", section: Just "Meat", common: false}
    DB.insert container {name: "Italian seasoning", store: "ALDI", section: Just "Spices", common: true}
    DB.insert container {name: "Jalapeno", store: "ALDI", section: Just "Produce", common: false}
    DB.insert container {name: "Kale", store: "ALDI", section: Just "Produce", common: true}
    DB.insert container {name: "Kidney beans", store: "ALDI", section: Just "Canned goods", common: true}
    DB.insert container {name: "Lettuce", store: "ALDI", section: Just "Produce", common: true}
    DB.insert container {name: "Limes", store: "Harris Teeter", section: Just "Produce", common: true}
    DB.insert container {name: "Liquid aminos", store: "Harris Teeter", section: Nothing, common: true}
    DB.insert container {name: "Mayonnaise", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Milk", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Monterey jack", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Mozzarella", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Mushrooms", store: "ALDI", section: Just "Produce", common: false}
    DB.insert container {name: "Nathans Wine", store: "Costco", section: Nothing, common: true}
    DB.insert container {name: "Navy beans", store: "ALDI", section: Just "Canned goods", common: false}
    DB.insert container {name: "Olive oil", store: "Costco", section: Nothing, common: true}
    DB.insert container {name: "Onion", store: "ALDI", section: Just "Produce", common: true}
    DB.insert container {name: "Paprika", store: "ALDI", section: Just "Spices", common: true}
    DB.insert container {name: "Parsley", store: "ALDI", section: Just "Produce", common: false}
    DB.insert container {name: "Peas", store: "ALDI", section: Just "Frozen", common: true}
    DB.insert container {name: "Pepper", store: "ALDI", section: Just "Spices", common: true}
    DB.insert container {name: "Pickles", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Poblano pepper", store: "Harris Teeter", section: Just "Produce", common: false}
    DB.insert container {name: "Potatoes", store: "ALDI", section: Just "Produce", common: true}
    DB.insert container {name: "Red curry paste", store: "Harris Teeter", section: Just "International", common: true}
    DB.insert container {name: "Rice", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Rice vinegar", store: "Harris Teeter", section: Nothing, common: true}
    DB.insert container {name: "Salt", store: "ALDI", section: Just "Spices", common: true}
    DB.insert container {name: "Scallions", store: "ALDI", section: Just "Produce", common: false}
    DB.insert container {name: "Sesame oil", store: "Harris Teeter", section: Nothing, common: true}
    DB.insert container {name: "Sesame seeds", store: "Harris Teeter", section: Nothing, common: true}
    DB.insert container {name: "Sour cream", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Southwest spice blend", store: "ALDI", section: Just "Spices", common: true}
    DB.insert container {name: "Spaghetti", store: "Costco", section: Nothing, common: true}
    DB.insert container {name: "Split peas", store: "ALDI", section: Nothing, common: false}
    DB.insert container {name: "Sriracha", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Stuffing mix", store: "ALDI", section: Nothing, common: false}
    DB.insert container {name: "Sugar", store: "Costco", section: Nothing, common: true}
    DB.insert container {name: "Swiss cheese", store: "ALDI", section: Just "Dairy", common: true}
    DB.insert container {name: "Tomato sauce", store: "ALDI", section: Just "Canned goods", common: false}
    DB.insert container {name: "Tomato paste", store: "ALDI", section: Just "Canned goods", common: false}
    DB.insert container {name: "Tomatoes", store: "ALDI", section: Just "Produce", common: true}
    DB.insert container {name: "Tortillas", store: "ALDI", section: Nothing, common: true}
    DB.insert container {name: "Tumeric", store: "Harris Teeter", section: Just "Spices", common: true}
    DB.insert container {name: "White wine vinegar", store: "Harris Teeter", section: Nothing, common: true}

    log "Populated ingredients"