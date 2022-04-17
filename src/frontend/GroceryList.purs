module Recipes.Frontend.GroceryList where

import Frontend.Prelude

import Affjax.RequestBody as RequestBody
import Concur.React.Props (unsafeTargetValue)
import Concur.React.Props as Props
import Data.HTTP.Method (Method(..))
import Data.List as List
import Data.List.NonEmpty as NEList
import Data.List.Types (List, NonEmptyList)
import Recipes.API (AddItemValue, RecipeRoute(..), SetItemStatusValue)
import Recipes.API as Routing
import Recipes.Frontend.Http (expectRequest)
import Recipes.Frontend.IngredientList (ingredientListItem)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (confirm, location)


sectionList :: NonEmptyList SetItemStatusValue -> Widget HTML SetItemStatusValue
sectionList itemsBySection = 
  h4' [text section]
  <|>
  fold (itemsBySection <#> ingredientListItem)
  
  where 
    section = fromMaybe "Misc." (NEList.head itemsBySection).item.ingredient.section 

storeList :: NonEmptyList SetItemStatusValue -> Widget HTML SetItemStatusValue
storeList itemsByStore = 
  h2' [text store]
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
      { textDecoration: "underline", marginTop: "1em", marginBottom: "1em", border: "none", backgroundColor: "white", color: "#444" }
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

data GroceryListStep 
  = UpdateList (List SetItemStatusValue) 
  | ReloadThePage 

type GroceryListAction = Maybe GroceryListStep

groceryItems :: List SetItemStatusValue -> Widget HTML GroceryListAction
groceryItems items = do
  newCheckedItem <- intercalate hr' (byStore items <#> storeList)

  liftEffect $ checkItem newCheckedItem
  let updatedItems = items <#> \oldItem -> if oldItem.item.ingredient.name == newCheckedItem.item.ingredient.name then newCheckedItem else oldItem
  pure $ Just $ UpdateList updatedItems

  where 
    byStore :: List SetItemStatusValue -> List $ NonEmptyList SetItemStatusValue
    byStore = 
      List.sortBy (comparing _.item.ingredient.store)
      >>> List.groupBy (equating _.item.ingredient.store)

    checkItem item = launchAff_ $ expectRequest $ defaultRequest 
      { method = Left POST, url = Routing.print SetItemStatus
      , content = Just $ RequestBody.Json $ encodeJson item
      }

addItemToListForm :: Widget HTML GroceryListAction
addItemToListForm = do
  newItem <- addItemForm 
  liftAff $ submitItem newItem
  pure $ Just ReloadThePage

  where

    submitItem item = 
      expectRequest $ defaultRequest
        { method = Left POST, url = Routing.print AddItem
        , content = Just $ RequestBody.Json $ encodeJson item
        }

resetGroceryListButton :: Widget HTML GroceryListAction
resetGroceryListButton = do
  div' [button [Props.onClick] [text "Restart"]] # void
  continue <- liftEffect (window >>= confirm "Are you sure you wish to reset the grocery list?")
  if not continue 
  then pure Nothing
  else do 
    liftAff resetState
    pure $ Just ReloadThePage 

  where
    resetState = expectRequest $ defaultRequest { url = Routing.print ResetState }


groceryList :: ∀ a. List SetItemStatusValue -> Widget HTML a
groceryList items = do 
  fold [ groceryItems items, addItemToListForm, resetGroceryListButton ]
  >>= case _ of
    Nothing -> groceryList items
    Just (UpdateList newList) -> groceryList newList
    Just ReloadThePage -> do
      liftEffect (window >>= location >>= reload)
      groceryList items

