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
import Recipes.Frontend.IngredientList (StoreItemSelection(..), ingredientListItem)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)


sectionList :: NonEmptyList SetItemStatusValue -> Widget HTML SetItemStatusValue
sectionList itemsBySection = 
  h4' [text section]
  <>
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
            [ td' [textBox [Props.value name] <#> state {name = _} <#> Left ]
            , td' [textBox [Props.value store] <#> state {store = _} <#> Left ]
            , td' [textBox [Props.value section] <#> state {section = _} <#> Left ]
            ]
          ]
        , br'
        , button [Props.onClick $> Right state, Props.style {marginBottom: "1em"}] [text "Done"]
        ]

      case formResult of
        Left newState -> itemInfo newState
        Right newState -> pure newState

    textBox props = input (props <> [Props._type "text", Props.onChange <#> unsafeTargetValue])


groceryList :: List SetItemStatusValue -> Widget HTML Unit
groceryList items = do 
  action <- 
    fold 
      [ h1' [text "Grocery list"]
      , fold (byStore items <#> (\itemsForStore -> storeList itemsForStore <#> (Right <<< AnotherItem)))
      , (addItemForm <#> Left)
      , (div' [button [Props.onClick] [text "Restart"]] $> Right FinishWithList)
      ]

  case action of 
    Right FinishWithList -> do
      liftAff resetState
      liftEffect (window >>= location >>= reload)
      groceryList items 

    Right (AnotherItem item) -> do
      liftAff $ checkItem item
      let updatedItems = items <#> \oldItem -> if oldItem.item.ingredient.name == item.item.ingredient.name then item else oldItem
      groceryList updatedItems

    Left item -> do
      liftAff $ submitItem item
      liftEffect (window >>= location >>= reload)
      groceryList items

  where 
    resetState = do 
      tryResp <- request $ defaultRequest { url = routeStr resetStateRoute }
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

    byStore :: List SetItemStatusValue -> List $ NonEmptyList SetItemStatusValue
    byStore = 
      List.sortBy (comparing _.item.ingredient.store)
      >>> List.groupBy (equating _.item.ingredient.store)

    submitItem item = do 
      tryResp <- request $ defaultRequest
        { method = Left POST, url = routeStr addItemRoute, responseFormat = ResponseFormat.string 
        , content = Just $ RequestBody.Json $ encodeJson item
        }

      resp <- tryResp # lmap printError # liftError
      if between 200 299 $ unwrap resp.status 
      then pure unit
      else throw (i"status "(show $ unwrap resp.status)". "(resp.body) :: String)
      