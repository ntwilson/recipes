module Recipes.DataStructures where

import Shared.Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..))
import Data.List as List
import Data.String (Pattern(..), split)
import Data.String as String
import Recipes.ErrorHandling (throw)

type Recipe = Record RecipeRow
type RecipeRow =
  ( name :: String
  , fullDescription :: String
  )

type Ingredient = Record IngredientRow
type IngredientRow =
  ( name :: String
  , store :: String
  , section :: Maybe String
  , common :: Boolean
  )

type RecipeIngredients = Record RecipeIngredientsRow
type RecipeIngredientsRow =
  ( recipe :: String
  , ingredient :: String
  , quantity :: Number
  , units :: Maybe String
  )

type Settings = Record SettingsRow
type SettingsRow =  
  ( name :: String
  , value :: String
  )

type SerializedAppState = Record AppStateRow
type AppStateRow = 
  ( name :: String
  , ingredients :: Maybe String 
  )

type StoreItem = { ingredient :: Ingredient, amount :: String }
data AppState = InputRecipes | CheckKitchen (List StoreItem) | BuyGroceries (List StoreItem)
derive instance eqAppState :: Eq AppState
derive instance genericAppState :: Generic AppState _ 
instance showAppState :: Show AppState where show = genericShow

decodeStoreItem :: ∀ m a. Throws String m a => List Ingredient -> String -> m StoreItem 
decodeStoreItem allIngredients item 
  | [amount, name] <- split (Pattern ":") item =  
    case List.find (_.name >>> (==) name) allIngredients of 
      Just ingredient -> pure { amount, ingredient }
      Nothing -> throw $ "Unable to find the ingredient named " <> name

  | otherwise = throw $ "Unable to decode app state item " <> item
  

decodeStoreItems :: ∀ m a. Throws String m a => List Ingredient -> Maybe String -> m $ List StoreItem
decodeStoreItems _ Nothing = pure Nil
decodeStoreItems allIngredients (Just serializedIngredients) = 
  split (Pattern ";") serializedIngredients 
  # Array.filter (not <<< String.null)
  # traverse (decodeStoreItem allIngredients)
  <#> List.fromFoldable

decodeAppState :: ∀ m a. Throws String m a => List Ingredient -> SerializedAppState -> m AppState 
decodeAppState allIngredients { name, ingredients: encodedIngredients } 
  | name == "input recipes" = pure InputRecipes
  | name == "check kitchen" = do
    ingredients <- decodeStoreItems allIngredients encodedIngredients
    pure $ CheckKitchen ingredients

  | name == "buy groceries" = do
    ingredients <- decodeStoreItems allIngredients encodedIngredients
    pure $ BuyGroceries ingredients

  | otherwise = throw $ "Unrecognized Program State: " <> name

encodeIngredients :: List StoreItem -> String
encodeIngredients ingredients = ingredients <#> (\{ingredient, amount} -> i(amount)":"(ingredient.name)) # intercalate ";"

encodeAppState :: AppState -> SerializedAppState
encodeAppState InputRecipes = { name: "input recipes", ingredients: Nothing }
encodeAppState (CheckKitchen ingredients) = { name: "check kitchen", ingredients: Just $ encodeIngredients ingredients }
encodeAppState (BuyGroceries ingredients) = { name: "buy groceries", ingredients: Just $ encodeIngredients ingredients }

