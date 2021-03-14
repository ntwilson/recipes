module Recipes.API where

import Shared.Prelude

import Data.Foldable (intercalate)
import Recipes.DataStructures (RecipeIngredients)

recipesRoute :: Array String
recipesRoute = ["api", "recipes"]
type RecipesValue = Array String

testRoute :: Array String
testRoute = ["api", "test"]
type TestValue = Array (Record RecipeIngredients)


routeStr :: Array String -> String
routeStr segments = i "/"(intercalate "/" segments)

