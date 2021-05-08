module Recipes.Frontend.PantryList where

import Frontend.Prelude

import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Props as Props
import Data.HTTP.Method (Method(..))
import Data.List (List)
import Data.List as List
import Recipes.API (SetItemStatusValue, routeStr, setItemStatusRoute, submitPantryRoute)
import Recipes.Frontend.IngredientList (StoreItemSelection(..), ingredientListItem)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

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
      liftAff progressState
      liftEffect (window >>= location >>= reload)

    AnotherItem item -> do
      liftAff $ checkItem item
      let updatedItems = items <#> \oldItem -> if oldItem.item.ingredient.name == item.item.ingredient.name then item else oldItem
      pantryList updatedItems


  where 
    commonItems = items # List.filter (_.item.ingredient.common)

    progressState = do 
      tryResp <- request $ defaultRequest { url = routeStr submitPantryRoute }
      resp <- tryResp # lmap printError # liftError
      if between 200 299 $ unwrap resp.status 
      then pure unit
      else throw (i"status "(show $ unwrap resp.status)"." :: String)

    checkItem item = do
      tryResp <- request $ defaultRequest 
        { method = Left POST, url = routeStr setItemStatusRoute, responseFormat = ResponseFormat.string 
        , content = Just $ RequestBody.Json $ encodeJson item
        }

      resp <- tryResp # lmap printError # liftError
      if between 200 299 $ unwrap resp.status 
      then pure unit
      else throw (i"status "(show $ unwrap resp.status)". "(resp.body) :: String)

