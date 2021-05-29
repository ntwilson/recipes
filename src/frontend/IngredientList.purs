module Recipes.Frontend.IngredientList where

import Frontend.Prelude

import Concur.React.Props as Props
import Recipes.API (SetItemStatusValue)

ingredientListItem :: SetItemStatusValue -> Widget HTML SetItemStatusValue
ingredientListItem storeItem = do
  div'  
    [ input [Props._type "checkbox", void Props.onChange, Props.checked storeItem.checked] 
    , span [void Props.onClick, Props.className "checkbox-text"] [text (i storeItem.item.amount" "storeItem.item.ingredient.name)]
    ]
  
  pure $ storeItem { checked = not storeItem.checked }
