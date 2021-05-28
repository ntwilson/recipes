module Recipes.Backend.StateSerialization where

import Backend.Prelude

import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.String (Pattern(..), split, stripPrefix)
import Data.String as String
import Recipes.DataStructures (AppState, CurrentUseCase(..), Ingredient, SerializedAppState, ShoppingState(..), StoreItem)

decodeCustomItem :: ∀ m a. Throws String m a => String -> m StoreItem
decodeCustomItem item 
  | [name, store, section] <- split (Pattern ":") item =  
    pure { amount: "", ingredient: { name, store, section: if section == "" then Nothing else Just section, common: false } }

  | otherwise = throw $ "Unable to decode app state item " <> item
  
decodeCustomItems :: ∀ m a. Throws String m a => Maybe String -> m $ List StoreItem 
decodeCustomItems Nothing = pure Nil
decodeCustomItems (Just serializedIngredients) = 
  split (Pattern ";") serializedIngredients 
  # Array.mapMaybe (stripPrefix $ Pattern "CUSTOM::")
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
    startsWith prefix = isJust <<< stripPrefix (Pattern prefix)

decodeAppState :: ∀ m a. Throws String m a => List Ingredient -> SerializedAppState -> m AppState 
decodeAppState allIngredients { name, ingredients: encodedIngredients } 
  | name == "input recipes" = pure {useCase: Shopping, shoppingState: InputRecipes, cookingState: Nothing}
  | name == "check kitchen" = do
    ingredients <- decodeStoreItems allIngredients encodedIngredients
    pure {useCase: Shopping, shoppingState: CheckKitchen ingredients, cookingState: Nothing}

  | name == "buy groceries" = do
    ingredients <- decodeStoreItems allIngredients encodedIngredients
    custom <- decodeCustomItems encodedIngredients
    pure {useCase: Shopping, shoppingState: BuyGroceries ingredients custom, cookingState: Nothing}

  | otherwise = throw $ "Unrecognized Program State: " <> name

encodeIngredients :: List StoreItem -> List StoreItem -> String
encodeIngredients ingredients customItems = (encodedNormalIngredients <> encodedCustomIngredients) # intercalate ";"
  where 
  encodedNormalIngredients = ingredients <#> (\{ingredient, amount} -> i(amount)":"(ingredient.name)) 
  encodedCustomIngredients = customItems <#> (\{ingredient, amount} -> 
    i"CUSTOM::"(ingredient.name)":"(ingredient.store)":"(fromMaybe "" ingredient.section))

encodeShopping :: ShoppingState -> Maybe String  
encodeShopping InputRecipes = Nothing
encodeShopping (CheckKitchen ingredients) = Just $ encodeIngredients ingredients Nil
encodeShopping (BuyGroceries ingredients custom) = Just $ encodeIngredients ingredients custom

encodeAppState :: AppState -> SerializedAppState
encodeAppState {useCase: Shopping, shoppingState: shopping@InputRecipes, cookingState} = 
  { name: "input recipes", ingredients: encodeShopping shopping, recipeSteps: Nothing }
encodeAppState {useCase: Shopping, shoppingState: shopping@(CheckKitchen _), cookingState} = 
  { name: "check kitchen", ingredients: encodeShopping shopping, recipeSteps: Nothing }
encodeAppState {useCase: Shopping, shoppingState: shopping@(BuyGroceries _ _), cookingState} = 
  { name: "buy groceries", ingredients: encodeShopping shopping, recipeSteps: Nothing }

encodeAppState ({useCase: Cooking, cookingState, shoppingState}) = 
  { name: "cooking", ingredients: encodeShopping shoppingState, recipeSteps: Nothing }
