module Recipes.RecipesToIngredients where

import Backend.Prelude

import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.NonEmpty (foldl1)
import Recipes.DataStructures (Ingredient, RecipeIngredients)

recipesToIngredients :: List RecipeIngredients -> List Ingredient -> List String -> List { amount :: String, ingredient :: Ingredient }
recipesToIngredients recipeIngredients allIngredients recipes = aggregatedIngredients
  where
    totalIngredients :: List RecipeIngredients
    totalIngredients = 
      recipeIngredients 
      # List.filter (\pair -> List.elem pair.recipe recipes)

    groupedIngredients :: List {ingredient :: String, amounts :: NonEmptyList RecipeIngredients}
    groupedIngredients = 
      totalIngredients 
      # List.sortBy (comparing _.ingredient) 
      # List.groupBy (\a b -> a.ingredient == b.ingredient)
      <#> \group -> {ingredient: (NEList.head group).ingredient, amounts: group}

    aggregatedIngredients = 
      groupedIngredients 
      <#> (\{ingredient, amounts} -> {ingredient, amount: aggregateGroup amounts})
      # List.mapMaybe (\{amount, ingredient: i} -> 
        case List.find ((_.name) >>> (==) i) allIngredients of
          Nothing -> Nothing
          Just ingredient -> Just {amount, ingredient}
      )

aggregateGroup :: ∀ r. NonEmptyList { quantity::Number, units::Maybe String | r} -> String
aggregateGroup ingredients = 
  ingredients 
  # NEList.sortBy (comparing _.units)
  # NEList.groupBy (\a b -> a.units == b.units)
  <#> (unwrap >>> foldl1 (\a b -> a { quantity = a.quantity + b.quantity }))
  <#> (\{quantity, units} -> i(show quantity)(maybe "" (" " <> _) units))
  # intercalate ", "
      


