module Recipes.Backend.DB.Setup where

import Backend.Prelude

import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Promise (Promise, toAff)
import Effect.Exception (message)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Recipes.Backend.DB (Container, CosmosClient, Database, PartitionKeyDefinition, connectionConfig, ingredientsContainer, newClient, newConnection, newPartitionKeyDef, recipeContainer, recipeIngredientsContainer)
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

  deleteContainer db "recipeIngredients" # handleFFI # runExceptT # void # lift
  void $ handleFFI $ runEffectFn3 createContainer db "recipeIngredients" $ newPartitionKeyDef "/recipe"
  log "Created recipeIngredients container"

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
  populateRecipeIngredients conn

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

  populateRecipeIngredients conn = do
    container <- recipeIngredientsContainer conn
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Carrots", quantity: 24.0, units: Just "oz"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Scallions", quantity: 4.0, units: Nothing}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Ginger", quantity: 0.75, units: Just "tbsp"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Mayonnaise", quantity: 4.0, units: Just "tbsp"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Sour cream", quantity: 4.0, units: Just "tbsp"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Honey", quantity: 4.0, units: Just "tsp"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Liquid aminos", quantity: 4.0, units: Just "tbsp"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Sriracha", quantity: 2.0, units: Just "tsp"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Sesame seeds", quantity: 2.0, units: Just "tbsp"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Rice", quantity: 1.0, units: Just "cups"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Ground beef", quantity: 20.0, units: Just "oz"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Almond flour", quantity: 0.5, units: Just "cups"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Chili flakes", quantity: 1.0, units: Just "tsp"}
    DB.insert container {recipe: "Firecracker Meatballs", ingredient: "Olive oil", quantity: 1.0, units: Just "tbsp"}

    DB.insert container {recipe: "Butter Chicken", ingredient: "Onion", quantity: 2.0, units: Just "cups"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Garlic", quantity: 5.0, units: Just "cloves"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Butter", quantity: 4.0, units: Just "tbsp"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Chicken", quantity: 2.0, units: Just "lbs"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Tomato sauce", quantity: 15.0, units: Just "oz"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Tomato paste", quantity: 3.0, units: Just "tbsp"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Red curry paste", quantity: 2.0, units: Just "tbsp"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Garam masala", quantity: 2.0, units: Just "tsp"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Ginger", quantity: 1.5, units: Just "tsp"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Salt", quantity: 1.0, units: Just "tsp"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Paprika", quantity: 0.5, units: Just "tsp"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Heavy cream", quantity: 0.5, units: Just "cups"}
    DB.insert container {recipe: "Butter Chicken", ingredient: "Cilantro", quantity: 1.0, units: Nothing}

    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Tomatoes", quantity: 2.0, units: Nothing}
    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Chives", quantity: 1.0, units: Nothing}
    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Parsley", quantity: 1.0, units: Nothing}
    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Chicken", quantity: 20.0, units: Just "oz"}
    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Italian seasoning", quantity: 1.0, units: Just "tbsp"}
    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Mozzarella", quantity: 1.0, units: Just "cups"}
    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Spaghetti", quantity: 12.0, units: Just "oz"}
    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Balsamic vinegar", quantity: 5.0, units: Just "tsp"}
    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Butter", quantity: 8.0, units: Just "tbsp"}
    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Olive oil", quantity: 2.0, units: Just "tsp"}
    DB.insert container {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Sugar", quantity: 0.5, units: Just "tsp"}

    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Onion", quantity: 1.0, units: Nothing}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Poblano pepper", quantity: 1.0, units: Nothing}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Cilantro", quantity: 0.5, units: Just "oz"}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Tomatoes", quantity: 2.0, units: Nothing}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Limes", quantity: 1.0, units: Nothing}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "White wine vinegar", quantity: 5.0, units: Just "tsp"}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Ground pork", quantity: 20.0, units: Just "oz"}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Southwest spice blend", quantity: 1.0, units: Just "tbsp"}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Chicken stock", quantity: 1.0, units: Nothing}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Tomato paste", quantity: 1.5, units: Just "oz"}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Sour cream", quantity: 4.0, units: Just "tbsp"}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Chipotle powder", quantity: 1.0, units: Just "tsp"}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Tortillas", quantity: 12.0, units: Nothing}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Monterey jack", quantity: 0.5, units: Just "cups"}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Olive oil", quantity: 2.0, units: Just "tsp"}
    DB.insert container {recipe: "Pork Carnitas Tacos", ingredient: "Sugar", quantity: 2.0, units: Just "tsp"}

    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Olive oil", quantity: 1.0, units: Just "tsp"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Garlic", quantity: 3.0, units: Just "cloves"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Onion", quantity: 2.0, units: Just "cups"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Chicken stock", quantity: 1.5, units: Just "boxes"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Chicken", quantity: 1.0, units: Just "lbs"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Navy beans", quantity: 1.0, units: Just "cans"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Great Northern beans", quantity: 1.0, units: Just "cans"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Diced tomatoes", quantity: 1.0, units: Just "cans"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Frozen corn", quantity: 1.0, units: Just "bags"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Colby cheese", quantity: 0.5, units: Just "cups"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Monterey jack", quantity: 0.5, units: Just "cups"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Cream cheese", quantity: 1.5, units: Just "bricks"}
    DB.insert container {recipe: "Creamy White Chicken Chili", ingredient: "Chicken bouillon", quantity: 2.0, units: Just "tsp"}

    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Italian sausage", quantity: 18.0, units: Just "oz"}
    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Bacon", quantity: 5.0, units: Just "strips"}
    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Chicken stock", quantity: 2.0, units: Just "cups"}
    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Onion", quantity: 1.0, units: Nothing}
    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Potatoes", quantity: 4.0, units: Nothing}
    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Garlic", quantity: 5.0, units: Just "cloves"}
    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Italian seasoning", quantity: 1.0, units: Just "dash"}
    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Heavy cream", quantity: 1.0, units: Just "cups"}
    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Kale", quantity: 1.0, units: Just "bunch"}
    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Salt", quantity: 1.0, units: Just "pinch"}
    DB.insert container {recipe: "Zuppa Toscana", ingredient: "Pepper", quantity: 1.0, units: Just "pinch"}

    DB.insert container {recipe: "Simple Chili", ingredient: "Chili starter", quantity: 2.0, units: Just "cans"}
    DB.insert container {recipe: "Simple Chili", ingredient: "Kidney beans", quantity: 2.0, units: Just "cans"}
    DB.insert container {recipe: "Simple Chili", ingredient: "Navy beans", quantity: 2.0, units: Just "cans"}
    DB.insert container {recipe: "Simple Chili", ingredient: "Diced tomatoes", quantity: 2.0, units: Just "cans"}
    DB.insert container {recipe: "Simple Chili", ingredient: "Ground beef", quantity: 2.0, units: Just "lbs"}

    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Chicken", quantity: 3.0, units: Just "lbs"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Greek yogurt", quantity: 1.5, units: Just "cups"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Garlic", quantity: 8.0, units: Just "cloves"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Ginger", quantity: 4.0, units: Just "tsp"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Garam masala", quantity: 2.0, units: Just "tbsp"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Cumin", quantity: 1.0, units: Just "tsp"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Salt", quantity: 1.0, units: Just "tsp"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Paprika", quantity: 2.0, units: Just "tsp"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Onion", quantity: 1.5, units: Just "cups"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Tomato paste", quantity: 1.0, units: Just "tbsp"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Diced tomatoes", quantity: 1.0, units: Just "cans"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Coriander", quantity: 2.0, units: Just "tsp"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Tumeric", quantity: 0.5, units: Just "tsp"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Coconut milk", quantity: 1.0, units: Just "can"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Cilantro", quantity: 1.0, units: Just "bunch"}
    DB.insert container {recipe: "Chicken Tikka Masala", ingredient: "Cayenne pepper", quantity: 1.0, units: Just "pinch"}

    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Chicken", quantity: 1.0, units: Just "lbs"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Lettuce", quantity: 8.0, units: Just "leaves"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Scallions", quantity: 0.5, units: Just "cups"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Mushrooms", quantity: 1.0, units: Just "cups"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Onion", quantity: 0.5, units: Nothing}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Garlic", quantity: 4.0, units: Just "cloves"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Ginger", quantity: 0.5, units: Just "in"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Jalapeno", quantity: 1.0, units: Nothing}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Chili flakes", quantity: 0.5, units: Just "tbsp"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Olive oil", quantity: 3.0, units: Just "tbsp"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Hoisin sauce", quantity: 2.0, units: Just "tbsp"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Sriracha", quantity: 2.0, units: Just "tbsp"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Liquid aminos", quantity: 3.0, units: Just "tbsp"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Rice vinegar", quantity: 1.5, units: Just "tbsp"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Brown sugar", quantity: 3.0, units: Just "tbsp"}
    DB.insert container {recipe: "Chicken Lettuce Wraps", ingredient: "Sesame oil", quantity: 2.0, units: Just "tbsp"}

    DB.insert container {recipe: "Swiss Chicken", ingredient: "Butter", quantity: 0.25, units: Just "cups"}
    DB.insert container {recipe: "Swiss Chicken", ingredient: "Chicken", quantity: 4.0, units: Nothing}
    DB.insert container {recipe: "Swiss Chicken", ingredient: "Swiss cheese", quantity: 6.0, units: Just "slices"}
    DB.insert container {recipe: "Swiss Chicken", ingredient: "Cream of chicken", quantity: 1.0, units: Just "cans"}
    DB.insert container {recipe: "Swiss Chicken", ingredient: "Milk", quantity: 0.5, units: Just "cups"}
    DB.insert container {recipe: "Swiss Chicken", ingredient: "Stuffing mix", quantity: 1.0, units: Just "boxes"}
    DB.insert container {recipe: "Swiss Chicken", ingredient: "Chicken stock", quantity: 0.5, units: Just "cups"}

    DB.insert container {recipe: "Split Pea Soup", ingredient: "Olive oil", quantity: 2.0, units: Just "tbsp"}
    DB.insert container {recipe: "Split Pea Soup", ingredient: "Onion", quantity: 2.0, units: Just "cups"}
    DB.insert container {recipe: "Split Pea Soup", ingredient: "Celery", quantity: 2.0, units: Just "cups"}
    DB.insert container {recipe: "Split Pea Soup", ingredient: "Carrots", quantity: 2.0, units: Just "cups"}
    DB.insert container {recipe: "Split Pea Soup", ingredient: "Cauliflower rice", quantity: 2.0, units: Just "cups"}
    DB.insert container {recipe: "Split Pea Soup", ingredient: "Ham", quantity: 1.25, units: Just "cups"}
    DB.insert container {recipe: "Split Pea Soup", ingredient: "Garlic", quantity: 3.0, units: Just "cloves"}
    DB.insert container {recipe: "Split Pea Soup", ingredient: "Chicken stock", quantity: 8.0, units: Just "cups"}
    DB.insert container {recipe: "Split Pea Soup", ingredient: "Split peas", quantity: 0.5, units: Just "lbs"}
    DB.insert container {recipe: "Split Pea Soup", ingredient: "Salt", quantity: 1.0, units: Just "tsp"}
    DB.insert container {recipe: "Split Pea Soup", ingredient: "Bay leaves", quantity: 3.0, units: Nothing}

    DB.insert container {recipe: "The Staples", ingredient: "Half & half", quantity: 2.0, units: Just "cartons"}
    DB.insert container {recipe: "The Staples", ingredient: "Pickles", quantity: 2.0, units: Just "jars"}
    DB.insert container {recipe: "The Staples", ingredient: "Chips", quantity: 2.0, units: Just "bags"}
    DB.insert container {recipe: "The Staples", ingredient: "Annikas Wine", quantity: 2.0, units: Just "bottles"}
    DB.insert container {recipe: "The Staples", ingredient: "Nathans Wine", quantity: 2.0, units: Just "bottles"}
    DB.insert container {recipe: "The Staples", ingredient: "Eggs", quantity: 1.0, units: Just "carton"}
    DB.insert container {recipe: "The Staples", ingredient: "Cream cheese", quantity: 2.0, units: Just "bricks"}
    log "Populated recipeIngredients"
