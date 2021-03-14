module Recipes.DataStructures where

import Shared.Prelude

type Recipe =
  ( name :: String
  , fullDescription :: String
  )

type Ingredient =
  ( name :: String
  , store :: String
  , section :: Maybe String
  , common :: Boolean
  )

type RecipeIngredients =
  ( recipe :: String
  , ingredient :: String
  , quantity :: Number
  , units :: Maybe String
  )

type Settings = 
  ( name :: String
  , value :: String
  )