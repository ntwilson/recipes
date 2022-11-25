module Recipes.DataStructures where

import Shared.Prelude

import Data.Array as Array
import Data.Argonaut.Core as Json
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Compat as Codec.Compat
import Data.Codec.Argonaut.Generic as Codec
import Data.Codec.Argonaut.Record as Codec.Record
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Foreign.Object as Object

type Ingredient = { name :: String, store :: String, section :: Maybe String, common :: Boolean }

ingredientCodec :: JsonCodec Ingredient
ingredientCodec = Codec.Record.object "Ingredient"
  { name: Codec.string, store: Codec.string, section: Codec.Compat.maybe Codec.string, common: Codec.boolean }

knownIngredientCodec :: List Ingredient -> JsonCodec Ingredient
knownIngredientCodec allIngredients = basicCodec decode encode
  where
  encode = Codec.encode ingredientCodec
  decode json = do
    ingredient <- Codec.decode ingredientCodec json
    case List.find (_.name >>> (==) ingredient.name) allIngredients of
      Just ingredient -> pure ingredient
      Nothing -> throwError $ Codec.TypeMismatch $ "Unable to find the ingredient named " <> ingredient.name

type RecipeIngredients = { recipe :: String, ingredient :: String, quantity :: Number, units :: Maybe String }


type RecipeSteps = { recipeName :: String, stepNumber :: Int, stepDescription :: String }

type RecipeStep = { completed :: Boolean, ordinal :: Int, description :: String }
type CookingState = { recipe :: String, steps :: List RecipeStep }

recipeStepCodec :: JsonCodec RecipeStep
recipeStepCodec = Codec.Record.object "RecipeStep" { completed: Codec.boolean, ordinal: Codec.int, description: Codec.string }
cookingStateCodec :: JsonCodec CookingState
cookingStateCodec = Codec.Record.object "CookingState" { recipe: Codec.string, steps: listCodec recipeStepCodec }

type StoreItem = { ingredient :: Ingredient, amount :: String }

storeItemCodec :: JsonCodec StoreItem
storeItemCodec = Codec.Record.object "StoreItem" { ingredient: ingredientCodec, amount: Codec.string }

knownStoreItemCodec :: List Ingredient -> JsonCodec StoreItem
knownStoreItemCodec allIngredients = basicCodec decode encode
  where
  encode = Codec.encode storeItemCodec
  decode json = do
    {ingredient, amount} <- Codec.decode storeItemCodec json
    ingredient <- Codec.decode (knownIngredientCodec allIngredients) $ Codec.encode ingredientCodec ingredient
    pure { ingredient, amount }

data ShoppingState 
  = InputRecipes 
  | CheckKitchen (List StoreItem) 
  | BuyGroceries (List StoreItem) (List StoreItem)

derive instance Eq ShoppingState
derive instance Generic ShoppingState _ 
instance Show ShoppingState where show = genericShow

listCodec :: ∀ a. JsonCodec a -> JsonCodec (List a)
listCodec = dimap Array.fromFoldable List.fromFoldable <<< Codec.array

shoppingStateCodec :: List Ingredient -> JsonCodec ShoppingState
shoppingStateCodec allIngredients = basicCodec decode encode
  where 
  checkKitchenCodec = listCodec storeItemCodec
  buyGroceriesCodec = Codec.Record.object "BuyGroceries" 
    { storeList: listCodec $ knownStoreItemCodec allIngredients
    , customItems: listCodec storeItemCodec
    }

  encode = case _ of
    InputRecipes -> Json.jsonSingletonObject "inputRecipes" Json.jsonEmptyObject
    CheckKitchen items -> Json.jsonSingletonObject "checkKitchen" $ Codec.encode checkKitchenCodec items
    BuyGroceries storeList customItems -> Json.jsonSingletonObject "buyGroceries" $ 
      Codec.encode buyGroceriesCodec {storeList, customItems} 

  decode json = do
    obj <- Json.toObject json # note (Codec.TypeMismatch "Object")
    go obj
    where
      go obj
        | Just _ <- Object.lookup "inputRecipes" obj = pure InputRecipes
        | Just storeList <- Object.lookup "checkKitchen" obj = CheckKitchen <$> Codec.decode checkKitchenCodec storeList
        | Just storeLists <- Object.lookup "buyGroceries" obj = do
          { storeList, customItems } <- Codec.decode buyGroceriesCodec storeLists
          pure $ BuyGroceries storeList customItems
        | otherwise = throwError $ Codec.TypeMismatch "`{inputRecipes:_}|{checkKitchen:_}|{buyGroceries:_}"

data CurrentUseCase = Shopping | Cooking 
derive instance Eq CurrentUseCase
derive instance Generic CurrentUseCase _ 
instance Show CurrentUseCase where show = genericShow
useCaseCodec :: JsonCodec CurrentUseCase
useCaseCodec = Codec.nullarySum "CurrentUseCase"

type AppState = { useCase :: CurrentUseCase, shoppingState :: ShoppingState, cookingState :: Maybe CookingState }
appStateCodec :: List Ingredient -> JsonCodec AppState
appStateCodec allIngredients = 
  Codec.Record.object "AppState"
    { useCase: useCaseCodec
    , shoppingState: shoppingStateCodec allIngredients
    , cookingState: Codec.Compat.maybe cookingStateCodec
    }

