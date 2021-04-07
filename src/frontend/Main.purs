module Recipes.Frontend.Main where

import Frontend.Prelude

import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Data.Argonaut (printJsonDecodeError)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NEList
import Data.List.Types (NonEmptyList)
import Recipes.API (RecipesValue, SetItemStatusValue, currentStateRoute, ingredientsRoute, recipesRoute, resetStateRoute, routeStr, setItemStatusRoute, submitRecipesRoute)
import Recipes.DataStructures (AppState(..), Ingredient, decodeAppState)
import Recipes.ErrorHandling (throw)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

loadRecipes :: Aff RecipesValue
loadRecipes = do
  resp <- request $ defaultRequest { url = routeStr recipesRoute, responseFormat = ResponseFormat.json }
  {body} <- resp # lmap printError # liftError
  decodeJson body # lmap printJsonDecodeError # liftError

submitRecipes :: List String -> Aff Unit
submitRecipes recipes = do
  tryResp <- request $ defaultRequest 
    { method = Left POST, url = routeStr submitRecipesRoute, responseFormat = ResponseFormat.string 
    , content = Just $ RequestBody.Json $ encodeJson recipes
    }

  resp <- tryResp # lmap printError # liftError
  if between 200 299 $ unwrap resp.status 
  then pure unit
  else throw (i"status "(show $ unwrap resp.status)". "(resp.body) :: String)

recipeList :: ∀ f. Traversable f => f String -> Widget HTML String
recipeList allRecipes = 
  fold (allRecipes <#> \recipe -> div' [input [Props._type "checkbox", Props.onChange $> recipe] <|> text recipe])

data RecipeSelection = AnotherRecipe String | SubmitRecipes
selectedRecipes :: ∀ f. Traversable f => f String -> List String -> Widget HTML $ List String
selectedRecipes allRecipes selectedRecipesSoFar = do
  selection <- 
    fold
      [ recipeList allRecipes <#> AnotherRecipe
      , br'
      , div' [button [Props.onClick] [text "Submit"]] $> SubmitRecipes
      ]

  case selection of 
    SubmitRecipes -> pure selectedRecipesSoFar
    AnotherRecipe recipe -> selectedRecipes allRecipes $ updateSelection recipe

  where 
    updateSelection nextSelected = 
      if nextSelected `elem` selectedRecipesSoFar
      then List.delete nextSelected selectedRecipesSoFar
      else nextSelected : selectedRecipesSoFar

loadIngredients :: Aff $ List Ingredient
loadIngredients = do
  resp <- request $ defaultRequest { url = routeStr ingredientsRoute, responseFormat = ResponseFormat.json }
  {body} <- resp # lmap printError # liftError
  decodeJson body # lmap printJsonDecodeError # liftError

loadState :: Aff AppState 
loadState = do
  resp <- request $ defaultRequest { url = routeStr currentStateRoute, responseFormat = ResponseFormat.json }
  {body} <- resp # lmap printError # liftError
  serialized <- decodeJson body # lmap printJsonDecodeError # liftError
  ingredients <- loadIngredients
  decodeAppState ingredients serialized

inputRecipes :: Widget HTML Unit 
inputRecipes = do 
  recipes <- (text "Loading..." <|> liftAff loadRecipes)
  selected <- selectedRecipes recipes Nil
  liftAff $ submitRecipes selected
  liftEffect (window >>= location >>= reload)

groceryListItem :: SetItemStatusValue -> Widget HTML SetItemStatusValue
groceryListItem storeItem = do
  changeEvent <- 
    ( div' [input [Props._type "checkbox", Props.onChange, Props.checked storeItem.checked] 
    <> text (i storeItem.item.amount" "storeItem.item.ingredient.name)]
    )
  
  pure $ storeItem { checked = not storeItem.checked }

data StoreItemSelection = AnotherItem SetItemStatusValue | ResetStoreList

sectionList :: NonEmptyList SetItemStatusValue -> Widget HTML SetItemStatusValue
sectionList itemsBySection = 
  h4' [text section]
  <>
  fold (itemsBySection <#> groceryListItem)
  
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

groceryList :: List SetItemStatusValue -> Widget HTML Unit
groceryList items = do 
  action <- 
    ( fold (byStore items <#> (\itemsForStore -> storeList itemsForStore <#> AnotherItem))
    <> br'
    <> (div' [button [Props.onClick] [text "Restart"]] $> ResetStoreList)
    )

  case action of 
    ResetStoreList -> do
      liftAff resetState
      liftEffect (window >>= location >>= reload)
      groceryList items 

    AnotherItem item -> do
      liftAff $ checkItem item
      let updatedItems = items <#> \oldItem -> if oldItem.item.ingredient.name == item.item.ingredient.name then item else oldItem
      groceryList updatedItems


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


content :: Widget HTML Unit
content = do
  appState <- (text "Loading..." <|> liftAff loadState)
  case appState of 
    InputRecipes -> inputRecipes
    CheckKitchen ingredients -> groceryList (ingredients <#> {checked: false, item: _})
    BuyGroceries ingredients -> groceryList (ingredients <#> {checked: false, item: _})

main :: Effect Unit
main = runWidgetInDom "contents" content
