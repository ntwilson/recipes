module Recipes.Frontend.GroceryList where

import Frontend.Prelude

import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Props (unsafeTargetValue)
import Concur.React.Props as Props
import Data.HTTP.Method (Method(..))
import Data.List as List
import Data.List.NonEmpty as NEList
import Data.List.Types (List, NonEmptyList)
import Recipes.API (AddItemValue, SetItemStatusValue, addItemRoute, resetStateRoute, routeStr, setItemStatusRoute)
import Recipes.Frontend.Http (expectRequest)
import Recipes.Frontend.IngredientList (ingredientListItem)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)


sectionList :: NonEmptyList SetItemStatusValue -> Widget HTML SetItemStatusValue
sectionList itemsBySection = 
  h4' [text section]
  <|>
  fold (itemsBySection <#> ingredientListItem)
  
  where 
    section = fromMaybe "Misc." (NEList.head itemsBySection).item.ingredient.section 

storeList :: NonEmptyList SetItemStatusValue -> Widget HTML SetItemStatusValue
storeList itemsByStore = 
  h3' [text store]
  <>
  fold (bySection (NEList.toList itemsByStore) <#> sectionList)

  where
    store = (NEList.head itemsByStore).item.ingredient.store

    bySection :: List SetItemStatusValue -> List $ NonEmptyList SetItemStatusValue
    bySection = 
      List.sortBy (comparing _.item.ingredient.section)
      >>> List.groupBy (equating _.item.ingredient.section)

type AddItemState = {name::String, store::String, section::String}
data AddItemAction = EnterState AddItemState | Finish AddItemState
addItemForm :: Widget HTML AddItemValue
addItemForm = do
  _ <- button 
    [ Props.onClick 
    , Props.style 
      { textDecoration: "underline", marginTop: "1em", marginBottom: "1em", border: "none", backgroundColor: "white" }
    ] 
    [text "Add another item"]

  { name, store, section } <- itemInfo {name: "", store: "", section: ""}

  pure { name, store, section: if section == "" then Nothing else Just section, common: false }

  where 
    itemInfo :: {name::String, store::String, section::String} -> Widget HTML {name::String, store::String, section::String}
    itemInfo state@{name, store, section} = do
      formResult <- fold 
        [ table [ Props.style { marginTop: "1em" } ]
          [ tr' [ td' [text "Name"], td' [text "Store"], td' [text "Section (optional)"] ]
          , tr' 
            [ td' [textBox [Props.value name] <#> state {name = _} <#> EnterState ]
            , td' [textBox [Props.value store] <#> state {store = _} <#> EnterState ]
            , td' [textBox [Props.value section] <#> state {section = _} <#> EnterState ]
            ]
          ]
        , br'
        , button [Props.onClick $> Finish state, Props.style {marginBottom: "1em"}] [text "Done"]
        ]

      case formResult of
        EnterState newState -> itemInfo newState
        Finish newState -> pure newState

    textBox props = input (props <> [Props._type "text", Props.onChange <#> unsafeTargetValue])

data GroceryListAction = NewItem AddItemValue | CheckItem SetItemStatusValue | Finished 
groceryList :: List SetItemStatusValue -> Widget HTML Unit
groceryList items = do 
  action <- 
    fold 
      [ h1' [text "Grocery list"]
      , fold (byStore items <#> (\itemsForStore -> storeList itemsForStore <#> CheckItem))
      , (addItemForm <#> NewItem)
      , (div' [button [Props.onClick] [text "Restart"]] $> Finished)
      ]

  case action of 
    Finished -> do
      liftAff resetState
      liftEffect (window >>= location >>= reload)
      groceryList items 

    CheckItem item -> do
      liftEffect $ checkItem item
      let updatedItems = items <#> \oldItem -> if oldItem.item.ingredient.name == item.item.ingredient.name then item else oldItem
      groceryList updatedItems

    NewItem item -> do
      liftAff $ submitItem item
      liftEffect (window >>= location >>= reload)
      groceryList items

  where 
    resetState = expectRequest $ defaultRequest { url = routeStr resetStateRoute }

    checkItem item = launchAff_ $ expectRequest $ defaultRequest 
      { method = Left POST, url = routeStr setItemStatusRoute
      , content = Just $ RequestBody.Json $ encodeJson item
      }

    submitItem item = 
      expectRequest $ defaultRequest
        { method = Left POST, url = routeStr addItemRoute
        , content = Just $ RequestBody.Json $ encodeJson item
        }

    byStore :: List SetItemStatusValue -> List $ NonEmptyList SetItemStatusValue
    byStore = 
      List.sortBy (comparing _.item.ingredient.store)
      >>> List.groupBy (equating _.item.ingredient.store)
