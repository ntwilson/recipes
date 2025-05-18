module Recipes.Frontend.PantryList where

import Frontend.Prelude

import Affjax.RequestBody as RequestBody
import Concur.React.Props as Props
import Data.HTTP.Method (Method(..))
import Data.List (List)
import Data.List as List
import Recipes.API (RecipeRoute(..), SetItemStatusValue, setItemStatusCodec)
import Recipes.API as Routing
import Recipes.Frontend.ExceptVWidget (ExceptVWidget(..))
import Recipes.Frontend.Http (expectRequest)
import Recipes.Frontend.IngredientList (ingredientListItem)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

data StoreItemSelection = AnotherItem SetItemStatusValue | FinishWithList
pantryList :: ∀ r. List SetItemStatusValue -> ExceptVWidget (STRING_ERROR r) HTML Unit
pantryList items = do 
  action <- 
    fold 
      [ h3' [text "Are any of these in your kitchen already?"]
      , fold (commonItems <#> (\itemsForStore -> ingredientListItem itemsForStore <#> AnotherItem))
      , br'
      , (div' [button [Props.onClick] [text "Ready to shop"]] $> FinishWithList)
      ]

  case action of 
    FinishWithList -> do
      ExceptVWidget $ expectRequest $ defaultRequest { url = Routing.print SubmitPantry }
      liftEffect (window >>= location >>= reload)

    AnotherItem item -> do
      ExceptVWidget $ checkItem item
      let updatedItems = items <#> \oldItem -> if oldItem.item.ingredient.name == item.item.ingredient.name then item else oldItem
      pantryList updatedItems


  where 
    commonItems = items # List.filter (_.item.ingredient.common)

    checkItem item = expectRequest $ defaultRequest 
      { method = Left POST, url = Routing.print SetItemStatus
      , content = Just $ RequestBody.Json $ encode setItemStatusCodec item
      }
