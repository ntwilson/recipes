module Recipes.Frontend.PantryList where

import Frontend.Prelude

import Affjax.RequestBody as RequestBody
import Concur.React.Props as Props
import Data.HTTP.Method (Method(..))
import Data.List (List)
import Data.List as List
import Recipes.API (SetItemStatusValue, routeStr, setItemStatusRoute, submitPantryRoute)
import Recipes.Frontend.Http (expectRequest)
import Recipes.Frontend.IngredientList (ingredientListItem)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

data StoreItemSelection = AnotherItem SetItemStatusValue | FinishWithList
pantryList :: List SetItemStatusValue -> Widget HTML Unit
pantryList items = do 
  action <- 
    fold 
      [ h1' [text "Are any of these in your kitchen already?"]
      , fold (commonItems <#> (\itemsForStore -> ingredientListItem itemsForStore <#> AnotherItem))
      , br'
      , (div' [button [Props.onClick] [text "Ready to shop"]] $> FinishWithList)
      ]

  case action of 
    FinishWithList -> do
      liftAff $ expectRequest $ defaultRequest { url = routeStr submitPantryRoute }
      liftEffect (window >>= location >>= reload)

    AnotherItem item -> do
      liftEffect $ checkItem item
      let updatedItems = items <#> \oldItem -> if oldItem.item.ingredient.name == item.item.ingredient.name then item else oldItem
      pantryList updatedItems


  where 
    commonItems = items # List.filter (_.item.ingredient.common)

    checkItem item = launchAff_ $ expectRequest $ defaultRequest 
      { method = Left POST, url = routeStr setItemStatusRoute
      , content = Just $ RequestBody.Json $ encodeJson item
      }
