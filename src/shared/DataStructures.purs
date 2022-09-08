module Recipes.DataStructures where

import Shared.Prelude

import Data.Argonaut (Json, JsonDecodeError(..))
import Data.Argonaut as Json
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Show.Generic (genericShow)

type Recipe = { name :: String, fullDescription :: String }

type Ingredient = { name :: String, store :: String, section :: Maybe String, common :: Boolean }
decodeKnownIngredient :: List Ingredient -> Json -> Either JsonDecodeError Ingredient
decodeKnownIngredient allIngredients json = do
  (ingredient :: Ingredient) <- decodeJson json
  case List.find (_.name >>> (==) ingredient.name) allIngredients of 
    Just ingredient -> pure ingredient
    Nothing -> throw $ TypeMismatch $ "Unable to find the ingredient named " <> ingredient.name

decodeCustomIngredient :: Json -> Either JsonDecodeError Ingredient
decodeCustomIngredient = decodeJson 

type RecipeIngredients = { recipe :: String, ingredient :: String, quantity :: Number, units :: Maybe String }


type RecipeSteps = { recipeName :: String, stepNumber :: Int, stepDescription :: String }
type RecipeStepsDB = { recipename :: String, stepnumber :: Int, stepdescription :: String }

type SerializedAppState = { name :: String, ingredients :: Maybe String, recipeSteps :: Maybe String }
type SerializedAppStateDB = { name :: String, ingredients :: Maybe String, recipesteps :: Maybe String }


type RecipeStep = { completed :: Boolean, ordinal :: Int, description :: String }
type CookingState = { recipe :: String, steps :: List RecipeStep }

type StoreItem = { ingredient :: Ingredient, amount :: String }
decodeKnownStoreItem :: List Ingredient -> Json -> Either JsonDecodeError StoreItem
decodeKnownStoreItem allIngredients json = do 
  ({ingredient, amount} :: {ingredient::_, amount::_}) <- decodeJson json
  ingredient <- decodeKnownIngredient allIngredients ingredient
  pure {ingredient, amount}

decodeCustomStoreItem :: Json -> Either JsonDecodeError StoreItem
decodeCustomStoreItem = decodeJson

data ShoppingState 
  = InputRecipes 
  | CheckKitchen (List StoreItem) 
  | BuyGroceries (List StoreItem) (List StoreItem)

derive instance Eq ShoppingState
derive instance Generic ShoppingState _ 
instance Show ShoppingState where show = genericShow

instance EncodeJson ShoppingState where
  encodeJson InputRecipes = encodeJson { name: "InputRecipes" }
  encodeJson (CheckKitchen storeList) = encodeJson { name: "CheckKitchen", storeList }
  encodeJson (BuyGroceries storeList customItems) = encodeJson { name: "BuyGroceries", storeList, customItems }

decodeShoppingState :: List Ingredient -> Json -> Either JsonDecodeError ShoppingState
decodeShoppingState allIngredients json  
  | Right ({ name: "InputRecipes" } :: { name :: String }) <- decodeJson json = Right InputRecipes 
  | Right ({ name: "CheckKitchen", storeList } :: { name :: String, storeList :: List Json }) <- decodeJson json = do
    storeList <- traverse (decodeKnownStoreItem allIngredients) storeList
    pure $ CheckKitchen storeList

  | Right ({ name: "BuyGroceries", storeList, customItems } :: { name :: String, storeList :: List Json, customItems :: List Json }) <- decodeJson json = do
    storeList <- traverse (decodeKnownStoreItem allIngredients) storeList
    customItems <- traverse decodeCustomStoreItem customItems
    pure $ BuyGroceries storeList customItems

  | otherwise = Left $ TypeMismatch "`{name:'InputRecipes'}` | `{name:'CheckKitchen', storeList:<StoreItem[]>}` | `{name:'BuyGroceries', storeList<StoreItem[]>, customItems:<StoreItem[]>}`"

data CurrentUseCase = Shopping | Cooking 
derive instance Eq CurrentUseCase
derive instance Generic CurrentUseCase _ 
instance Show CurrentUseCase where show = genericShow
instance DecodeJson CurrentUseCase where
  decodeJson json = case Json.toString json of
    Just "Shopping" -> Right Shopping
    Just "Cooking" -> Right Cooking
    Just _str -> Left $ TypeMismatch "value: 'Shopping'|'Cooking'"
    Nothing -> Left $ TypeMismatch "String"

instance EncodeJson CurrentUseCase where
  encodeJson Shopping = encodeJson "Shopping"
  encodeJson Cooking  = encodeJson "Cooking"

type AppState = { useCase :: CurrentUseCase, shoppingState :: ShoppingState, cookingState :: Maybe CookingState }
decodeAppState :: List Ingredient -> Json -> Either JsonDecodeError AppState
decodeAppState allIngredients json = do
  ({useCase, shoppingState, cookingState} :: { useCase::_, shoppingState::_, cookingState::_}) <- decodeJson json
  shoppingState <- decodeShoppingState allIngredients shoppingState
  pure { useCase, shoppingState, cookingState }



