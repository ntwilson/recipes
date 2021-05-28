module Recipes.DataStructures where

import Shared.Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)

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

type RecipeSteps = Record RecipeStepsRow
type RecipeStepsRow = 
  ( recipeName :: String
  , stepNumber :: Int
  , stepDescription :: String
  )

type SerializedAppState = Record AppStateRow
type AppStateRow = 
  ( name :: String
  , ingredients :: Maybe String 
  , recipeSteps :: Maybe String
  )


type RecipeStep = { completed :: Boolean, ordinal :: Int, description :: String }
type CookingState = { recipe :: String, steps :: List RecipeStep }

type StoreItem = { ingredient :: Ingredient, amount :: String }
data ShoppingState 
  = InputRecipes 
  | CheckKitchen (List StoreItem) 
  | BuyGroceries (List StoreItem) (List StoreItem)

derive instance eqShopping :: Eq ShoppingState
derive instance genericShopping :: Generic ShoppingState _ 
instance showShopping :: Show ShoppingState where show = genericShow

data CurrentUseCase = Shopping | Cooking 
derive instance eqCurrentUseCase :: Eq CurrentUseCase
derive instance genericCurrentUseCase :: Generic CurrentUseCase _ 
instance showCurrentUseCase :: Show CurrentUseCase where show = genericShow

type AppState = { useCase :: CurrentUseCase, shoppingState :: ShoppingState, cookingState :: Maybe CookingState }



