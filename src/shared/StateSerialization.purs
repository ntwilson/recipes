module Recipes.StateSerialization where

import Backend.Prelude

import Data.Array as Array
import Data.Int as Int
import Data.List (List(..))
import Data.List as List
import Data.String (Pattern(..), split, stripPrefix)
import Data.String as String
import Recipes.DataStructures (AppState, CookingState, CurrentUseCase(..), Ingredient, RecipeStep, SerializedAppState, ShoppingState(..), StoreItem)

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

decodeSteps :: ∀ m a. Throws String m a => String -> m (List RecipeStep)
decodeSteps steps = for (List.fromFoldable $ split (Pattern ";") steps) $ \step ->
  case split (Pattern "&&") step of 
    [encodedCompletion, encodedOrdinal, description]
    | Just completed <- decodeBool encodedCompletion, Just ordinal <- Int.fromString encodedOrdinal ->
      pure { completed, ordinal, description }

    [completion, ordinal, _description] -> throw (i"unrecognized completion/ordinal: '"completion"'/'"ordinal"'" :: String)
    _ -> throw (i"Invalid app state (recipe steps), expected completion, ordinal, description separated by '&&'. Got '"step"'" :: String)

  where 
    decodeBool "T" = Just true
    decodeBool "F" = Just false
    decodeBool _   = Nothing


decodeCookingState :: ∀ m a. Throws String m a => Maybe String -> m (Maybe CookingState)
decodeCookingState Nothing   = pure Nothing
decodeCookingState (Just "") = pure Nothing
decodeCookingState (Just cookingState) = 
  case split (Pattern "::") cookingState of
    ["", _] -> throw "Invalid AppState (Cooking state), recipe was blank"
    [_, ""] -> throw "Invalid AppState (Cooking state), steps were blank"
    [recipe, steps] -> do
      parsedSteps <- decodeSteps steps
      pure $ Just { recipe, steps: parsedSteps }
    _ -> throw "Invalid AppState (Cooking state), expecting a recipe, then '::' then the steps"

decodeAppState :: ∀ m a. Throws String m a => List Ingredient -> SerializedAppState -> m AppState 
decodeAppState allIngredients { name, ingredients: encodedIngredients, recipeSteps: encodedSteps } 
  | name == "input recipes" = do
    cookingState <- decodeCookingState encodedSteps
    pure {useCase: Shopping, shoppingState: InputRecipes, cookingState}
  | name == "check kitchen" = do
    ingredients <- decodeStoreItems allIngredients encodedIngredients
    cookingState <- decodeCookingState encodedSteps
    pure {useCase: Shopping, shoppingState: CheckKitchen ingredients, cookingState}

  | name == "buy groceries" = do
    ingredients <- decodeStoreItems allIngredients encodedIngredients
    custom <- decodeCustomItems encodedIngredients
    cookingState <- decodeCookingState encodedSteps
    pure {useCase: Shopping, shoppingState: BuyGroceries ingredients custom, cookingState}

  | name == "cooking(input recipes)" = do
    cookingState <- decodeCookingState encodedSteps
    pure {useCase: Cooking, shoppingState: InputRecipes, cookingState}
  | name == "cooking(check kitchen)" = do
    cookingState <- decodeCookingState encodedSteps
    ingredients <- decodeStoreItems allIngredients encodedIngredients
    pure {useCase: Cooking, shoppingState: CheckKitchen ingredients, cookingState}
  | name == "cooking(buy groceries)" = do
    cookingState <- decodeCookingState encodedSteps
    ingredients <- decodeStoreItems allIngredients encodedIngredients
    custom <- decodeCustomItems encodedIngredients
    pure {useCase: Cooking, shoppingState: BuyGroceries ingredients custom, cookingState}

  | otherwise = throw $ "Unrecognized Program State: " <> name

encodeIngredients :: List StoreItem -> List StoreItem -> String
encodeIngredients ingredients customItems = (encodedNormalIngredients <> encodedCustomIngredients) # intercalate ";"
  where 
  encodedNormalIngredients = ingredients <#> (\{ingredient, amount} -> i(amount)":"(ingredient.name)) 
  encodedCustomIngredients = customItems <#> (\{ingredient} -> 
    i"CUSTOM::"(ingredient.name)":"(ingredient.store)":"(fromMaybe "" ingredient.section))

encodeShopping :: ShoppingState -> Maybe String  
encodeShopping InputRecipes = Nothing
encodeShopping (CheckKitchen ingredients) = Just $ encodeIngredients ingredients Nil
encodeShopping (BuyGroceries ingredients custom) = Just $ encodeIngredients ingredients custom

encodeCooking :: CookingState -> String 
encodeCooking {recipe, steps} = i recipe"::"encodedSteps
  where 
    encodedSteps = intercalate ";" (encodeStep <$> steps)
    encodeStep {completed, ordinal, description} = i (encodeBool completed)"&&"(show ordinal)"&&"description
    encodeBool true  = "T"
    encodeBool false = "F"

encodeAppState :: AppState -> SerializedAppState
encodeAppState {useCase: Shopping, shoppingState: shopping@InputRecipes, cookingState} = 
  { name: "input recipes", ingredients: encodeShopping shopping, recipeSteps: encodeCooking <$> cookingState }
encodeAppState {useCase: Shopping, shoppingState: shopping@(CheckKitchen _), cookingState} = 
  { name: "check kitchen", ingredients: encodeShopping shopping, recipeSteps: encodeCooking <$> cookingState }
encodeAppState {useCase: Shopping, shoppingState: shopping@(BuyGroceries _ _), cookingState} = 
  { name: "buy groceries", ingredients: encodeShopping shopping, recipeSteps: encodeCooking <$> cookingState }

encodeAppState ({useCase: Cooking, cookingState, shoppingState}) = 
  { name, ingredients: encodeShopping shoppingState, recipeSteps: encodeCooking <$> cookingState }
  where 
    name = case shoppingState of 
      InputRecipes -> "cooking(input recipes)"
      CheckKitchen _ -> "cooking(check kitchen)"
      BuyGroceries _ _ -> "cooking(buy groceries)"
