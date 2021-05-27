module Recipes.Frontend.IngredientList where

import Frontend.Prelude

import Concur.React.Props as Props
import Recipes.API (SetItemStatusValue)

ingredientListItem :: SetItemStatusValue -> Widget HTML SetItemStatusValue
ingredientListItem storeItem = do
  changeEvent <- 
    div' 
      [ input [Props._type "checkbox", Props.onChange, Props.checked storeItem.checked] 
      , text (i storeItem.item.amount" "storeItem.item.ingredient.name)
      ]
  
  pure $ storeItem { checked = not storeItem.checked }
