module Recipes.DataStructures where

import Shared.Prelude

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