module Recipes.Backend.DB.Setup where

import Backend.Prelude

import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Promise (Promise, toAff)
import Data.List as List
import Effect.Exception (message)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn3)
import Recipes.Backend.DB (Container, CosmosClient, Database, PartitionKeyDefinition, appStatePartitionKey, ingredientsPartitionKey, newConnection, partitionKeyDef, printQueryError, recipeIngredientsPartitionKey, recipeStepsPartitionKey, recipesPartitionKey)
import Recipes.Backend.DB as DB
import Recipes.Backend.ServerSetup (loadEnv)
import Recipes.DataStructures (CurrentUseCase(..), ShoppingState(..))

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

  db <- newConnection
  deleteContainer db "recipes" # handleFFI # try # void
  void $ handleFFI $ runEffectFn3 createContainer db "recipes" $ partitionKeyDef recipesPartitionKey
  log "Created recipes container"

  deleteContainer db "ingredients" # handleFFI # try # void
  void $ handleFFI $ runEffectFn3 createContainer db "ingredients" $ partitionKeyDef ingredientsPartitionKey
  log "Created ingredients container"

  -- deleteContainer db "recipeIngredients" # handleFFI # try # void
  void $ handleFFI $ runEffectFn3 createContainer db "recipeIngredients" $ partitionKeyDef recipeIngredientsPartitionKey
  log "Created recipeIngredients container"

  -- deleteContainer db "recipeSteps" # handleFFI # try # void
  void $ handleFFI $ runEffectFn3 createContainer db "recipeSteps" $ partitionKeyDef recipeStepsPartitionKey
  log "Created recipeSteps container"

  void $ handleFFI $ runEffectFn3 createContainer db "appState" $ partitionKeyDef appStatePartitionKey
  log "Created appState container"

  where
  handleFFI :: ∀ a. Effect (Promise a) -> ExceptT String m a
  handleFFI fn = do
    promise <- withExceptT message $ ExceptT $ liftEffect $ try fn
    withExceptT message $ ExceptT $ liftAff $ try $ toAff promise

populateData :: ∀ m. MonadAff m => ExceptT String m Unit
populateData = do
  populateRecipes
  populateIngredients
  populateRecipeIngredients
  populateRecipeSteps
  populateAppState

  where
  populateAppState = do
    existingAppState <- DB.readAllAppStates (List.fromFoldable ingredients) # withExceptT printQueryError
    case existingAppState of
      [] -> do
        DB.insertAppState (List.fromFoldable ingredients) { useCase: Shopping, shoppingState: InputRecipes, cookingState: Nothing }
        log "Populated appState"
      _ -> log "AppState already populated"

  populateRecipes :: ExceptT String m Unit
  populateRecipes = do
    traverse_ DB.insertRecipe recipes
    log "Populated recipes"
  
  recipes = 
    [ {name: "Balsamic Tomato & Herb Chicken"}
    , {name: "Butter Chicken"}
    , {name: "Firecracker Meatballs"}
    , {name: "Pork Carnitas Tacos"}
    , {name: "Creamy White Chicken Chili"}
    , {name: "Zuppa Toscana"}
    , {name: "Simple Chili"}
    , {name: "Chicken Tikka Masala"}
    , {name: "Chicken Lettuce Wraps"}
    , {name: "Swiss Chicken"}
    , {name: "Split Pea Soup"}
    , {name: "The Staples"}
    ]

  populateIngredients :: ExceptT String m Unit
  populateIngredients = do
    traverse_ DB.insertIngredient ingredients
    log "Populated ingredients"
  
  ingredients = 
    [ {name: "Almond flour", store: "Harris Teeter", section: Nothing, common: true}
    , {name: "Annika's Wine", store: "ALDI", section: Nothing, common: true}
    , {name: "Bacon", store: "ALDI", section: Just "Meat", common: true}
    , {name: "Balsamic vinegar", store: "Costco", section: Nothing, common: true}
    , {name: "Bay leaves", store: "Harris Teeter", section: Just "Produce", common: false}
    , {name: "Brown sugar", store: "ALDI", section: Nothing, common: true}
    , {name: "Butter", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Carrots", store: "ALDI", section: Just "Produce", common: true}
    , {name: "Cauliflower rice", store: "ALDI", section: Just "Frozen", common: true}
    , {name: "Cayenne pepper", store: "ALDI", section: Just "Spices", common: true}
    , {name: "Celery", store: "ALDI", section: Just "Produce", common: true}
    , {name: "Chicken", store: "Costco", section: Just "Meat", common: false}
    , {name: "Chicken bouillon", store: "ALDI", section: Nothing, common: true}
    , {name: "Chicken stock", store: "ALDI", section: Nothing, common: true}
    , {name: "Chili flakes", store: "Harris Teeter", section: Just "Spices", common: true}
    , {name: "Chili starter", store: "ALDI", section: Just "Canned goods", common: true}
    , {name: "Chipotle powder", store: "Harris Teeter", section: Just "Spices", common: true}
    , {name: "Chips", store: "ALDI", section: Nothing, common: true}
    , {name: "Chives", store: "Harris Teeter", section: Just "Produce", common: false}
    , {name: "Cilantro", store: "ALDI", section: Just "Produce", common: true}
    , {name: "Coconut milk", store: "Harris Teeter", section: Just "Dairy", common: false}
    , {name: "Colby cheese", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Coriander", store: "Harris Teeter", section: Just "Spices", common: true}
    , {name: "Cream cheese", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Cream of chicken", store: "ALDI", section: Just "Canned goods", common: true}
    , {name: "Cumin", store: "ALDI", section: Just "Spices", common: true}
    , {name: "Diced tomatoes", store: "ALDI", section: Just "Canned goods", common: false}
    , {name: "Eggs", store: "Costco", section: Just "Dairy", common: true}
    , {name: "Frozen Corn", store: "ALDI", section: Just "Frozen", common: true}
    , {name: "Garam masala", store: "Harris Teeter", section: Just "Spices", common: true}
    , {name: "Garlic", store: "ALDI", section: Nothing, common: true}
    , {name: "Ginger", store: "Costco", section: Nothing, common: true}
    , {name: "Great Northern beans", store: "ALDI", section: Just "Canned goods", common: false}
    , {name: "Greek yogurt", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Ground beef", store: "ALDI", section: Just "Meat", common: false}
    , {name: "Ground pork", store: "ALDI", section: Just "Meat", common: false}
    , {name: "Ham", store: "ALDI", section: Just "Meat", common: false}
    , {name: "Half & half", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Heavy cream", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Hoisin sauce", store: "Harris Teeter", section: Nothing, common: true}
    , {name: "Honey", store: "ALDI", section: Nothing, common: true}
    , {name: "Italian sausage", store: "ALDI", section: Just "Meat", common: false}
    , {name: "Italian seasoning", store: "ALDI", section: Just "Spices", common: true}
    , {name: "Jalapeno", store: "ALDI", section: Just "Produce", common: false}
    , {name: "Kale", store: "ALDI", section: Just "Produce", common: true}
    , {name: "Kidney beans", store: "ALDI", section: Just "Canned goods", common: true}
    , {name: "Lettuce", store: "ALDI", section: Just "Produce", common: true}
    , {name: "Limes", store: "Harris Teeter", section: Just "Produce", common: true}
    , {name: "Liquid aminos", store: "Harris Teeter", section: Nothing, common: true}
    , {name: "Mayonnaise", store: "ALDI", section: Nothing, common: true}
    , {name: "Milk", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Monterey jack", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Mozzarella", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Mushrooms", store: "ALDI", section: Just "Produce", common: false}
    , {name: "Nathans Wine", store: "Costco", section: Nothing, common: true}
    , {name: "Navy beans", store: "ALDI", section: Just "Canned goods", common: false}
    , {name: "Olive oil", store: "Costco", section: Nothing, common: true}
    , {name: "Onion", store: "ALDI", section: Just "Produce", common: true}
    , {name: "Paprika", store: "ALDI", section: Just "Spices", common: true}
    , {name: "Parsley", store: "ALDI", section: Just "Produce", common: false}
    , {name: "Peas", store: "ALDI", section: Just "Frozen", common: true}
    , {name: "Pepper", store: "ALDI", section: Just "Spices", common: true}
    , {name: "Pickles", store: "ALDI", section: Nothing, common: true}
    , {name: "Poblano pepper", store: "Harris Teeter", section: Just "Produce", common: false}
    , {name: "Potatoes", store: "ALDI", section: Just "Produce", common: true}
    , {name: "Red curry paste", store: "Harris Teeter", section: Just "International", common: true}
    , {name: "Rice", store: "ALDI", section: Nothing, common: true}
    , {name: "Rice vinegar", store: "Harris Teeter", section: Nothing, common: true}
    , {name: "Salt", store: "ALDI", section: Just "Spices", common: true}
    , {name: "Scallions", store: "ALDI", section: Just "Produce", common: false}
    , {name: "Sesame oil", store: "Harris Teeter", section: Nothing, common: true}
    , {name: "Sesame seeds", store: "Harris Teeter", section: Nothing, common: true}
    , {name: "Sour cream", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Southwest spice blend", store: "ALDI", section: Just "Spices", common: true}
    , {name: "Spaghetti", store: "Costco", section: Nothing, common: true}
    , {name: "Split peas", store: "ALDI", section: Nothing, common: false}
    , {name: "Sriracha", store: "ALDI", section: Nothing, common: true}
    , {name: "Stuffing mix", store: "ALDI", section: Nothing, common: false}
    , {name: "Sugar", store: "Costco", section: Nothing, common: true}
    , {name: "Swiss cheese", store: "ALDI", section: Just "Dairy", common: true}
    , {name: "Tomato sauce", store: "ALDI", section: Just "Canned goods", common: false}
    , {name: "Tomato paste", store: "ALDI", section: Just "Canned goods", common: false}
    , {name: "Tomatoes", store: "ALDI", section: Just "Produce", common: true}
    , {name: "Tortillas", store: "ALDI", section: Nothing, common: true}
    , {name: "Tumeric", store: "Harris Teeter", section: Just "Spices", common: true}
    , {name: "White wine vinegar", store: "Harris Teeter", section: Nothing, common: true}
    ]

  populateRecipeIngredients :: ExceptT String m Unit
  populateRecipeIngredients = do
    traverse_ DB.insertRecipeIngredients recipeIngredients
    log "Populated recipeIngredients"

  recipeIngredients = 
    [ {recipe: "Firecracker Meatballs", ingredient: "Carrots", quantity: 24.0, units: Just "oz"}
    , {recipe: "Firecracker Meatballs", ingredient: "Scallions", quantity: 4.0, units: Nothing}
    , {recipe: "Firecracker Meatballs", ingredient: "Ginger", quantity: 0.75, units: Just "tbsp"}
    , {recipe: "Firecracker Meatballs", ingredient: "Mayonnaise", quantity: 4.0, units: Just "tbsp"}
    , {recipe: "Firecracker Meatballs", ingredient: "Sour cream", quantity: 4.0, units: Just "tbsp"}
    , {recipe: "Firecracker Meatballs", ingredient: "Honey", quantity: 4.0, units: Just "tsp"}
    , {recipe: "Firecracker Meatballs", ingredient: "Liquid aminos", quantity: 4.0, units: Just "tbsp"}
    , {recipe: "Firecracker Meatballs", ingredient: "Sriracha", quantity: 2.0, units: Just "tsp"}
    , {recipe: "Firecracker Meatballs", ingredient: "Sesame seeds", quantity: 2.0, units: Just "tbsp"}
    , {recipe: "Firecracker Meatballs", ingredient: "Rice", quantity: 1.0, units: Just "cups"}
    , {recipe: "Firecracker Meatballs", ingredient: "Ground beef", quantity: 20.0, units: Just "oz"}
    , {recipe: "Firecracker Meatballs", ingredient: "Almond flour", quantity: 0.5, units: Just "cups"}
    , {recipe: "Firecracker Meatballs", ingredient: "Chili flakes", quantity: 1.0, units: Just "tsp"}
    , {recipe: "Firecracker Meatballs", ingredient: "Olive oil", quantity: 1.0, units: Just "tbsp"}

    , {recipe: "Butter Chicken", ingredient: "Onion", quantity: 2.0, units: Just "cups"}
    , {recipe: "Butter Chicken", ingredient: "Garlic", quantity: 5.0, units: Just "cloves"}
    , {recipe: "Butter Chicken", ingredient: "Butter", quantity: 4.0, units: Just "tbsp"}
    , {recipe: "Butter Chicken", ingredient: "Chicken", quantity: 2.0, units: Just "lbs"}
    , {recipe: "Butter Chicken", ingredient: "Tomato sauce", quantity: 15.0, units: Just "oz"}
    , {recipe: "Butter Chicken", ingredient: "Tomato paste", quantity: 3.0, units: Just "tbsp"}
    , {recipe: "Butter Chicken", ingredient: "Red curry paste", quantity: 2.0, units: Just "tbsp"}
    , {recipe: "Butter Chicken", ingredient: "Garam masala", quantity: 2.0, units: Just "tsp"}
    , {recipe: "Butter Chicken", ingredient: "Ginger", quantity: 1.5, units: Just "tsp"}
    , {recipe: "Butter Chicken", ingredient: "Salt", quantity: 1.0, units: Just "tsp"}
    , {recipe: "Butter Chicken", ingredient: "Paprika", quantity: 0.5, units: Just "tsp"}
    , {recipe: "Butter Chicken", ingredient: "Heavy cream", quantity: 0.5, units: Just "cups"}
    , {recipe: "Butter Chicken", ingredient: "Cilantro", quantity: 1.0, units: Nothing}

    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Tomatoes", quantity: 2.0, units: Nothing}
    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Chives", quantity: 1.0, units: Nothing}
    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Parsley", quantity: 1.0, units: Nothing}
    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Chicken", quantity: 20.0, units: Just "oz"}
    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Italian seasoning", quantity: 1.0, units: Just "tbsp"}
    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Mozzarella", quantity: 1.0, units: Just "cups"}
    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Spaghetti", quantity: 12.0, units: Just "oz"}
    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Balsamic vinegar", quantity: 5.0, units: Just "tsp"}
    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Butter", quantity: 8.0, units: Just "tbsp"}
    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Olive oil", quantity: 2.0, units: Just "tsp"}
    , {recipe: "Balsamic Tomato & Herb Chicken", ingredient: "Sugar", quantity: 0.5, units: Just "tsp"}

    , {recipe: "Pork Carnitas Tacos", ingredient: "Onion", quantity: 1.0, units: Nothing}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Poblano pepper", quantity: 1.0, units: Nothing}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Cilantro", quantity: 0.5, units: Just "oz"}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Tomatoes", quantity: 2.0, units: Nothing}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Limes", quantity: 1.0, units: Nothing}
    , {recipe: "Pork Carnitas Tacos", ingredient: "White wine vinegar", quantity: 5.0, units: Just "tsp"}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Ground pork", quantity: 20.0, units: Just "oz"}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Southwest spice blend", quantity: 1.0, units: Just "tbsp"}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Chicken stock", quantity: 1.0, units: Nothing}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Tomato paste", quantity: 1.5, units: Just "oz"}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Sour cream", quantity: 4.0, units: Just "tbsp"}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Chipotle powder", quantity: 1.0, units: Just "tsp"}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Tortillas", quantity: 12.0, units: Nothing}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Monterey jack", quantity: 0.5, units: Just "cups"}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Olive oil", quantity: 2.0, units: Just "tsp"}
    , {recipe: "Pork Carnitas Tacos", ingredient: "Sugar", quantity: 2.0, units: Just "tsp"}

    , {recipe: "Creamy White Chicken Chili", ingredient: "Olive oil", quantity: 1.0, units: Just "tsp"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Garlic", quantity: 3.0, units: Just "cloves"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Onion", quantity: 2.0, units: Just "cups"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Chicken stock", quantity: 1.5, units: Just "boxes"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Chicken", quantity: 1.0, units: Just "lbs"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Navy beans", quantity: 1.0, units: Just "cans"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Great Northern beans", quantity: 1.0, units: Just "cans"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Diced tomatoes", quantity: 1.0, units: Just "cans"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Frozen corn", quantity: 1.0, units: Just "bags"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Colby cheese", quantity: 0.5, units: Just "cups"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Monterey jack", quantity: 0.5, units: Just "cups"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Cream cheese", quantity: 1.5, units: Just "bricks"}
    , {recipe: "Creamy White Chicken Chili", ingredient: "Chicken bouillon", quantity: 2.0, units: Just "tsp"}

    , {recipe: "Zuppa Toscana", ingredient: "Italian sausage", quantity: 18.0, units: Just "oz"}
    , {recipe: "Zuppa Toscana", ingredient: "Bacon", quantity: 5.0, units: Just "strips"}
    , {recipe: "Zuppa Toscana", ingredient: "Chicken stock", quantity: 2.0, units: Just "cups"}
    , {recipe: "Zuppa Toscana", ingredient: "Onion", quantity: 1.0, units: Nothing}
    , {recipe: "Zuppa Toscana", ingredient: "Potatoes", quantity: 4.0, units: Nothing}
    , {recipe: "Zuppa Toscana", ingredient: "Garlic", quantity: 5.0, units: Just "cloves"}
    , {recipe: "Zuppa Toscana", ingredient: "Italian seasoning", quantity: 1.0, units: Just "dash"}
    , {recipe: "Zuppa Toscana", ingredient: "Heavy cream", quantity: 1.0, units: Just "cups"}
    , {recipe: "Zuppa Toscana", ingredient: "Kale", quantity: 1.0, units: Just "bunch"}
    , {recipe: "Zuppa Toscana", ingredient: "Salt", quantity: 1.0, units: Just "pinch"}
    , {recipe: "Zuppa Toscana", ingredient: "Pepper", quantity: 1.0, units: Just "pinch"}

    , {recipe: "Simple Chili", ingredient: "Chili starter", quantity: 2.0, units: Just "cans"}
    , {recipe: "Simple Chili", ingredient: "Kidney beans", quantity: 2.0, units: Just "cans"}
    , {recipe: "Simple Chili", ingredient: "Navy beans", quantity: 2.0, units: Just "cans"}
    , {recipe: "Simple Chili", ingredient: "Diced tomatoes", quantity: 2.0, units: Just "cans"}
    , {recipe: "Simple Chili", ingredient: "Ground beef", quantity: 2.0, units: Just "lbs"}

    , {recipe: "Chicken Tikka Masala", ingredient: "Chicken", quantity: 3.0, units: Just "lbs"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Greek yogurt", quantity: 1.5, units: Just "cups"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Garlic", quantity: 8.0, units: Just "cloves"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Ginger", quantity: 4.0, units: Just "tsp"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Garam masala", quantity: 2.0, units: Just "tbsp"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Cumin", quantity: 1.0, units: Just "tsp"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Salt", quantity: 1.0, units: Just "tsp"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Paprika", quantity: 2.0, units: Just "tsp"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Onion", quantity: 1.5, units: Just "cups"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Tomato paste", quantity: 1.0, units: Just "tbsp"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Diced tomatoes", quantity: 1.0, units: Just "cans"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Coriander", quantity: 2.0, units: Just "tsp"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Tumeric", quantity: 0.5, units: Just "tsp"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Coconut milk", quantity: 1.0, units: Just "can"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Cilantro", quantity: 1.0, units: Just "bunch"}
    , {recipe: "Chicken Tikka Masala", ingredient: "Cayenne pepper", quantity: 1.0, units: Just "pinch"}

    , {recipe: "Chicken Lettuce Wraps", ingredient: "Chicken", quantity: 1.0, units: Just "lbs"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Lettuce", quantity: 8.0, units: Just "leaves"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Scallions", quantity: 0.5, units: Just "cups"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Mushrooms", quantity: 1.0, units: Just "cups"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Onion", quantity: 0.5, units: Nothing}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Garlic", quantity: 4.0, units: Just "cloves"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Ginger", quantity: 0.5, units: Just "in"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Jalapeno", quantity: 1.0, units: Nothing}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Chili flakes", quantity: 0.5, units: Just "tbsp"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Olive oil", quantity: 3.0, units: Just "tbsp"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Hoisin sauce", quantity: 2.0, units: Just "tbsp"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Sriracha", quantity: 2.0, units: Just "tbsp"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Liquid aminos", quantity: 3.0, units: Just "tbsp"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Rice vinegar", quantity: 1.5, units: Just "tbsp"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Brown sugar", quantity: 3.0, units: Just "tbsp"}
    , {recipe: "Chicken Lettuce Wraps", ingredient: "Sesame oil", quantity: 2.0, units: Just "tbsp"}

    , {recipe: "Swiss Chicken", ingredient: "Butter", quantity: 0.25, units: Just "cups"}
    , {recipe: "Swiss Chicken", ingredient: "Chicken", quantity: 4.0, units: Nothing}
    , {recipe: "Swiss Chicken", ingredient: "Swiss cheese", quantity: 6.0, units: Just "slices"}
    , {recipe: "Swiss Chicken", ingredient: "Cream of chicken", quantity: 1.0, units: Just "cans"}
    , {recipe: "Swiss Chicken", ingredient: "Milk", quantity: 0.5, units: Just "cups"}
    , {recipe: "Swiss Chicken", ingredient: "Stuffing mix", quantity: 1.0, units: Just "boxes"}
    , {recipe: "Swiss Chicken", ingredient: "Chicken stock", quantity: 0.5, units: Just "cups"}

    , {recipe: "Split Pea Soup", ingredient: "Olive oil", quantity: 2.0, units: Just "tbsp"}
    , {recipe: "Split Pea Soup", ingredient: "Onion", quantity: 2.0, units: Just "cups"}
    , {recipe: "Split Pea Soup", ingredient: "Celery", quantity: 2.0, units: Just "cups"}
    , {recipe: "Split Pea Soup", ingredient: "Carrots", quantity: 2.0, units: Just "cups"}
    , {recipe: "Split Pea Soup", ingredient: "Cauliflower rice", quantity: 2.0, units: Just "cups"}
    , {recipe: "Split Pea Soup", ingredient: "Ham", quantity: 1.25, units: Just "cups"}
    , {recipe: "Split Pea Soup", ingredient: "Garlic", quantity: 3.0, units: Just "cloves"}
    , {recipe: "Split Pea Soup", ingredient: "Chicken stock", quantity: 8.0, units: Just "cups"}
    , {recipe: "Split Pea Soup", ingredient: "Split peas", quantity: 0.5, units: Just "lbs"}
    , {recipe: "Split Pea Soup", ingredient: "Salt", quantity: 1.0, units: Just "tsp"}
    , {recipe: "Split Pea Soup", ingredient: "Bay leaves", quantity: 3.0, units: Nothing}

    , {recipe: "The Staples", ingredient: "Half & half", quantity: 2.0, units: Just "cartons"}
    , {recipe: "The Staples", ingredient: "Pickles", quantity: 2.0, units: Just "jars"}
    , {recipe: "The Staples", ingredient: "Chips", quantity: 2.0, units: Just "bags"}
    , {recipe: "The Staples", ingredient: "Annikas Wine", quantity: 2.0, units: Just "bottles"}
    , {recipe: "The Staples", ingredient: "Nathans Wine", quantity: 2.0, units: Just "bottles"}
    , {recipe: "The Staples", ingredient: "Eggs", quantity: 1.0, units: Just "carton"}
    , {recipe: "The Staples", ingredient: "Cream cheese", quantity: 2.0, units: Just "bricks"}
    ]

  populateRecipeSteps :: ExceptT String m Unit
  populateRecipeSteps = do
    traverse_ DB.insertRecipeSteps recipeSteps
    log "Populated recipeSteps"

  recipeSteps = 
    [ {recipeName: "Simple Chili", stepNumber: 1, stepDescription: "Brown the beef in a pot"}
    , {recipeName: "Simple Chili", stepNumber: 2, stepDescription: "Empty the rest of the ingredients into the pot"}

    , {recipeName: "Zuppa Toscana", stepNumber: 1,  stepDescription: "Gather: 16 oz italian sausage, 5 strips bacon, 2 cups chicken broth, 1 medium onion, 4 red potatoes, 5 cloves garlic, Italian seasoning, 1 cup heavy cream, 1 bunch kale, salt & pepper"}
    , {recipeName: "Zuppa Toscana", stepNumber: 2,  stepDescription: "Cut 5 strips bacon into small pieces and add them to your Instant Pot"}
    , {recipeName: "Zuppa Toscana", stepNumber: 3,  stepDescription: "If sausage is encased (1 pack), take the sausages out of their casings. Slice lengthwise to make a slit in their casings to get them out"}
    , {recipeName: "Zuppa Toscana", stepNumber: 4,  stepDescription: "Add (16 oz) sausage to your Instant Pot"}
    , {recipeName: "Zuppa Toscana", stepNumber: 5,  stepDescription: "Press the \"saute\" button and cook the sausages and bacon until they're crispy and browned"}
    , {recipeName: "Zuppa Toscana", stepNumber: 6,  stepDescription: "While the sausages and bacon cook, chop 1 onion and 4 potatoes, making sure to stir the bacon and sausages occasionally"}
    , {recipeName: "Zuppa Toscana", stepNumber: 7,  stepDescription: "When the sausages and bacon are nicely browned and crispy, add in 2 cups chicken stock and 4 cups water"}
    , {recipeName: "Zuppa Toscana", stepNumber: 8,  stepDescription: "Add the onion, potatoes, 5 cloves garlic, and a dash of Italian seasoning"}
    , {recipeName: "Zuppa Toscana", stepNumber: 9,  stepDescription: "Stir the soup and close the Instant Pot's lid. Make sure the valve is set to \"sealing\""}
    , {recipeName: "Zuppa Toscana", stepNumber: 10, stepDescription: "Cook for 8 minutes on high pressure. The Instant Pot will take about 10 minutes or so to come up to pressure"}
    , {recipeName: "Zuppa Toscana", stepNumber: 11, stepDescription: "Carefully do a quick pressure release and take off the lid"}
    , {recipeName: "Zuppa Toscana", stepNumber: 12, stepDescription: "Add 1 cup heavy cream and 1 bunch kale. Close the lid again and let the soup sit for another 5 minutes or so until the kale wilts"}
    , {recipeName: "Zuppa Toscana", stepNumber: 13, stepDescription: "Season with salt & pepper"}

    , {recipeName: "Butter Chicken", stepNumber: 1, stepDescription: "Set your instant pot to saute and add 4 tbsp. butter, 2 cups onion and 5 cloves garlic. Saute for approximately 5 minutes until the onions are tender"}
    , {recipeName: "Butter Chicken", stepNumber: 2, stepDescription: "Cut 2 lbs chicken into bite-size pieces"}
    , {recipeName: "Butter Chicken", stepNumber: 3, stepDescription: "Add the chicken, 15 oz tomato sauce, 3 tbsp tomato paste, 2 tbsp red curry paste, 2 tsp garam masala, 1.5 tsp ginger, 1 tsp salt, and 0.5 tsp paprika. Lock the lid and turn the pressure valve to seal it. Set to high pressure for 7 minutes"}
    , {recipeName: "Butter Chicken", stepNumber: 4, stepDescription: "When the timer goes off do a natural release for 5 minutes and then turn the valve on top to venting to let the remaining steam out of the pressure cooker"}
    , {recipeName: "Butter Chicken", stepNumber: 5, stepDescription: "Add 0.5 cup heavy cream and stir"}
    , {recipeName: "Butter Chicken", stepNumber: 6, stepDescription: "Optionally add cilantro as garnish"}

    , {recipeName: "Creamy White Chicken Chili", stepNumber: 1, stepDescription: "Gather: olive oil, 2-3 garlic cloves, 1-2 cup onion, 1.5 boxes chicken broth, 2 chicken breasts, can navy beans, can Great Northern beans, can diced tomatoes, bag frozen corn, 1 cup shredded Colby/Monterrey Jack, 1.5 bricks creat cheese, chicken bouillon, chicken seasoning, chili powder, cumin, cayenne pepper."}
    , {recipeName: "Creamy White Chicken Chili", stepNumber: 2, stepDescription: "Place the Instant Pot on saute setting. Add 2-3 cloves garlic and 1-2 cups onion. Cook for 2-3 minutes until both are soft."}
    , {recipeName: "Creamy White Chicken Chili", stepNumber: 3, stepDescription: "Add 1.5 boxes chicken broth. Next add 2 chicken breasts, 2 cans beans, 1 can diced tomatoes, 2 tsp chicken bouillon, dash chicken seasoning, 1 tsp chili powder, 1 tsp cumin, 1 tsp cayenne pepper."}
    , {recipeName: "Creamy White Chicken Chili", stepNumber: 4, stepDescription: "Close the pot and seal. Cook on manual, high pressure for 20 minutes."}
    , {recipeName: "Creamy White Chicken Chili", stepNumber: 5, stepDescription: "When finished, natural release for 10 minutes, then quick release."}
    , {recipeName: "Creamy White Chicken Chili", stepNumber: 6, stepDescription: "Open the pot and remove the chicken breasts. Shred the chicken using a knife and fork, and return the chicken to the Instant Pot."}
    , {recipeName: "Creamy White Chicken Chili", stepNumber: 7, stepDescription: "Place the Instant Pot on the saute function. Add 1.5 bricks cream cheese, 1 cup shredded Colby/Monterrey Jack, 1 bag frozen corn."}
    , {recipeName: "Creamy White Chicken Chili", stepNumber: 8, stepDescription: "Serve once corn is soft and cheese is melted."}
    ]