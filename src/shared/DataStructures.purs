module Recipes.DataStructures where

import Shared.Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..))
import Data.List as List
import Data.String (Pattern(..), split)
import Data.String as String

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
data AppState = InputRecipes | CheckKitchen (List StoreItem) | BuyGroceries (List StoreItem) (List StoreItem)
derive instance eqAppState :: Eq AppState
derive instance genericAppState :: Generic AppState _ 
instance showAppState :: Show AppState where show = genericShow

decodeCustomItem :: ∀ m a. Throws String m a => String -> m StoreItem 
decodeCustomItem item 
  | [name, store, section] <- split (Pattern ":") item =  
    pure { amount: "", ingredient: { name, store, section: if section == "" then Nothing else Just section, common: false } }

  | otherwise = throw $ "Unable to decode app state item " <> item
  
decodeCustomItems :: ∀ m a. Throws String m a => Maybe String -> m $ List StoreItem 
decodeCustomItems Nothing = pure Nil
decodeCustomItems (Just serializedIngredients) = 
  split (Pattern ";") serializedIngredients 
  # Array.mapMaybe (String.stripPrefix $ Pattern "CUSTOM::")
  # traverse decodeCustomItem
  <#> List.fromFoldable

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
  # Array.filter (not <<< startsWith "CUSTOM::")
  # traverse (decodeStoreItem allIngredients)
  <#> List.fromFoldable

  where 
    startsWith prefix = isJust <<< String.stripPrefix (Pattern prefix)

decodeAppState :: ∀ m a. Throws String m a => List Ingredient -> SerializedAppState -> m AppState 
decodeAppState allIngredients { name, ingredients: encodedIngredients } 
  | name == "input recipes" = pure InputRecipes
  | name == "check kitchen" = do
    ingredients <- decodeStoreItems allIngredients encodedIngredients
    pure $ CheckKitchen ingredients

  | name == "buy groceries" = do
    ingredients <- decodeStoreItems allIngredients encodedIngredients
    custom <- decodeCustomItems encodedIngredients
    pure $ BuyGroceries ingredients custom

  | otherwise = throw $ "Unrecognized Program State: " <> name

encodeIngredients :: List StoreItem -> List StoreItem -> String
encodeIngredients ingredients customItems = (encodedNormalIngredients <> encodedCustomIngredients) # intercalate ";"
  where 
  encodedNormalIngredients = ingredients <#> (\{ingredient, amount} -> i(amount)":"(ingredient.name)) 
  encodedCustomIngredients = customItems <#> (\{ingredient, amount} -> 
    i"CUSTOM::"(ingredient.name)":"(ingredient.store)":"(fromMaybe "" ingredient.section))

encodeAppState :: AppState -> SerializedAppState
encodeAppState InputRecipes = { name: "input recipes", ingredients: Nothing }
encodeAppState (CheckKitchen ingredients) = { name: "check kitchen", ingredients: Just $ encodeIngredients ingredients Nil }
encodeAppState (BuyGroceries ingredients custom) = { name: "buy groceries", ingredients: Just $ encodeIngredients ingredients custom }

