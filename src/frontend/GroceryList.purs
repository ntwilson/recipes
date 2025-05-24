module Recipes.Frontend.GroceryList where

import Frontend.Prelude

import Affjax.RequestBody as RequestBody
import Concur.React.Props as Props
import Control.MultiAlternative (orr)
import Data.HTTP.Method (Method(..))
import Data.List as List
import Data.List.NonEmpty as NEList
import Data.List.Types (List, NonEmptyList)
import React.SyntheticEvent (SyntheticMouseEvent)
import Recipes.API (AddItemValue, RecipeRoute(..), SetItemStatusValue, setItemStatusCodec)
import Recipes.API as Routing
import Recipes.DataStructures (ingredientCodec)
import Recipes.Frontend.ExceptVWidget (ExceptVWidget(..))
import Recipes.Frontend.Http (expectRequest)
import Recipes.Frontend.IngredientList (ingredientListItem)
import Recipes.Frontend.MUI (class ReactWidget, TextSize(..))
import Recipes.Frontend.MUI as MUI
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
    bySection = List.groupAllBy (comparing _.item.ingredient.section)

foreign import scrollToBottom :: Effect Unit

type AddItemState = {name::String, store::String, section::String}
data AddItemAction = EnterState AddItemState | Finish AddItemState
addItemForm :: ∀ w. ReactWidget w => MonadEffect w => w AddItemValue
addItemForm = do
  MUI.floatingActionButton 
    ( MUI.fabProps @Unit { onClick: \(_::SyntheticMouseEvent) -> unit, classes: {root: "add-item-fab"} }) 
    [MUI.addIcon]

  liftEffect scrollToBottom

  { name, store, section } <- itemInfo {name: "", store: "", section: ""}

  pure { name, store, section: if section == "" then Nothing else Just section, common: false }

  where 
    itemInfo :: {name::String, store::String, section::String} -> w {name::String, store::String, section::String}
    itemInfo state@{name, store, section} = do
      formResult <- orr 
        [ textField { label: "Name", value: name, onChange: EnterState <<< state {name = _}, autoFocus: true }
        , textField { label: "Store", value: store, onChange: EnterState <<< state {store = _}, autoFocus: false }
        , textField { label: "Section (optional)", value: section, onChange: EnterState <<< state {section = _}, autoFocus: false }
        , br'
        , button [Props.onClick $> Finish state, Props.style {marginBottom: "1em"}] [text "Done"]
        ]

      case formResult of
        EnterState newState -> itemInfo newState
        Finish newState -> pure newState

    textField :: { label :: String, value :: String, onChange :: String -> AddItemAction, autoFocus :: Boolean } -> w AddItemAction
    textField {label, value, onChange, autoFocus} =
      MUI.textField 
        ( MUI.textFieldProps
          { label, value, onChange, size: TextSmall, classes: { root: "text-with-margin" }, autoFocus }
        ) 
        []

data GroceryListStep 
  = UpdateList (List SetItemStatusValue) 
  | ReloadThePage 

type GroceryListAction = Maybe GroceryListStep

groceryItems :: ∀ w. LiftWidget HTML w => MonadEffect w => List SetItemStatusValue -> w GroceryListAction
groceryItems items = do
  newCheckedItem <- liftWidget $ intercalate hr' (byStore items <#> storeList)

  liftEffect $ checkItem newCheckedItem
  let updatedItems = items <#> \oldItem -> if oldItem.item.ingredient.name == newCheckedItem.item.ingredient.name then newCheckedItem else oldItem
  pure $ Just $ UpdateList updatedItems

  where 
    byStore :: List SetItemStatusValue -> List $ NonEmptyList SetItemStatusValue
    byStore = List.groupAllBy (comparing _.item.ingredient.store)

    checkItem item = launchAff_ $ logErrs $ expectRequest $ defaultRequest 
      { method = Left POST, url = Routing.print SetItemStatus
      , content = Just $ RequestBody.Json $ encode setItemStatusCodec item
      }

    logErrs = handleErrors { httpError: log }

addItemToListForm :: ExceptVWidget _ HTML GroceryListAction
addItemToListForm = do
  newItem <- addItemForm 
  submitItem newItem
  pure $ Just ReloadThePage

  where

    submitItem item = 
      ExceptVWidget $ expectRequest $ defaultRequest
        { method = Left POST, url = Routing.print AddItem
        , content = Just $ RequestBody.Json $ encode ingredientCodec item
        }

resetGroceryListButton :: ExceptVWidget _ HTML GroceryListAction
resetGroceryListButton = do
  div' [button [Props.onClick] [text "Restart"]] # void
  continue <- liftEffect (window >>= confirm "Are you sure you wish to reset the grocery list?")
  if not continue 
  then pure Nothing
  else do 
    ExceptVWidget resetState
    pure $ Just ReloadThePage

  where
    resetState = expectRequest $ defaultRequest { url = Routing.print ResetState }


groceryList :: ∀ a. List SetItemStatusValue -> ExceptVWidget _ HTML a
groceryList items = do 
  fold [ groceryItems items, addItemToListForm, resetGroceryListButton ]
  >>= case _ of
    Nothing -> groceryList items
    Just (UpdateList newList) -> groceryList newList
    Just ReloadThePage -> do
      liftEffect (window >>= location >>= reload)
      groceryList items

