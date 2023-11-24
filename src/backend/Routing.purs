module Recipes.Backend.Routing where

import Backend.Prelude

import Data.Argonaut.Core as Json
import Data.Argonaut.Parser as Json
import Data.Codec.Argonaut.Common as Codec
import Data.List (List(..), (:))
import Data.List as List
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import HTTPure (Method(..))
import HTTPure as HTTPure
import HTTPure.Body as Body
import Node.Encoding (Encoding(..))
import Recipes.API (AddItemValue, RecipeRoute(..), recipeRouteDuplex, routeStr, setItemStatusCodec)
import Recipes.Backend.LoadState (allIngredients, allRecipeIngredients, allRecipes, getRecipesWithSteps, getState, getSteps, setState)
import Recipes.Backend.RecipesToIngredients (recipesToIngredients)
import Recipes.DataStructures (CurrentUseCase(..), ShoppingState(..), appStateCodec, ingredientCodec, recipeStepCodec, useCaseCodec)
import Routing.Duplex as Routing


run :: String -> HTTPure.Request -> HTTPure.ResponseM
run dist rqst = 
  runBaseAff' $ catch handler do
    body <- liftAff $ Body.toString rqst.body
    router dist rqst body

  where
  handler err = do
    log (i"-----------\nServer Error: "err"\n-----------")
    HTTPure.internalServerError err


router :: String -> HTTPure.Request -> String -> Run (AFFECT + EXCEPT String ()) HTTPure.Response
router dist rqst body = rtr rqst.method $ Routing.parse recipeRouteDuplex $ routeStr rqst.path
  where
  rtr Get (Right Home) = serveHtml (i dist"/index.html")
  rtr Get (Right CSS) = serveCss (i dist"/index.css")
  rtr Get (Right JS) = serveJavascript (i dist"/main.js")

  rtr Get (Right Recipes) = do
    contents <- encode (Codec.array Codec.string) <$> allRecipes
    HTTPure.ok $ Json.stringify contents

  rtr Get (Right Ingredients) = do
    contents <- encode (Codec.list ingredientCodec) <$> allIngredients
    HTTPure.ok $ Json.stringify contents

  rtr Post (Right SubmitRecipes) = go 
    where 
      go
        | Right json <- Json.jsonParser body
        , Right (submittedRecipes :: List String) <- decode (Codec.list Codec.string) json = do
          pairings <- allRecipeIngredients
          ingredients <- allIngredients
          state <- getState
          let groceryList = recipesToIngredients pairings ingredients submittedRecipes
          setState { useCase: Shopping, shoppingState: CheckKitchen groceryList, cookingState: state.cookingState }
          HTTPure.noContent
        
        | otherwise = HTTPure.badRequest "Could not parse request body"

  rtr Get (Right CurrentState) = do
    state <- getState
    ingredients <- allIngredients
    HTTPure.ok $ Json.stringify $ encode (appStateCodec ingredients) state

  rtr Get (Right ResetState) = do
    state <- getState
    setState $ state { useCase = Shopping, shoppingState = InputRecipes }
    HTTPure.noContent

  rtr Get (Right SubmitPantry) = do
    state <- getState
    case state of 
      {useCase: Shopping, shoppingState: CheckKitchen items} -> do
        setState $ state {shoppingState = BuyGroceries items Nil}
        HTTPure.noContent
      _ -> HTTPure.conflict "No items can or will exist until recipes are input."

  rtr Post (Right SetItemStatus) = go
    where
      go 
        | Right json <- Json.jsonParser body
        , Right submittedItem <- decode setItemStatusCodec json = do
          state <- getState 
          case state of 
            {useCase:Shopping, shoppingState: CheckKitchen items} -> do
              setState $ state {shoppingState = CheckKitchen $ processItem submittedItem items}
              HTTPure.noContent 
            {useCase:Shopping, shoppingState: BuyGroceries items custom} -> do
              if submittedItem.isCustom 
              then setState $ state{shoppingState = BuyGroceries items (processItem submittedItem custom)}
              else setState $ state{shoppingState = BuyGroceries (processItem submittedItem items) custom}
              HTTPure.noContent
            _ -> HTTPure.conflict "No items can or will exist until recipes are input."

        | otherwise = HTTPure.badRequest "Could not parse request body"
      
      processItem { checked, item: submittedItem } items
        | checked = List.filter (_.ingredient.name >>> (/=) submittedItem.ingredient.name) items
        | otherwise = submittedItem : items

  rtr Post (Right AddItem) = go 
    where 
      go 
        | Right json <- Json.jsonParser body 
        , Right submittedItem <- decode ingredientCodec json = do
          state <- getState
          case state of
            {useCase: Shopping, shoppingState: BuyGroceries items custom} -> do
              setState $ state {shoppingState = BuyGroceries items (addItem submittedItem items custom)}
              HTTPure.noContent
            _ -> HTTPure.conflict "The application is not in a state such that adding items is permitted."

        | otherwise = HTTPure.badRequest "Could not parse request body"

      addItem ingredient normalItems customItems = { ingredient: corrected, amount: "" } : customItems
        where
          corrected = correctItem (normalItems <#> _.ingredient) (customItems <#> _.ingredient) ingredient

      correctItem :: List AddItemValue -> List AddItemValue -> AddItemValue -> AddItemValue 
      correctItem existingItems existingCustom newCustom = newCustom { store = correctedStore, section = correctedSection }
        where 
          correctedStore = case List.find (\{store: existingStore} -> (eq `on` CaseInsensitiveString) existingStore newCustom.store) allExisting of
            Just existingItem -> existingItem.store
            Nothing -> newCustom.store

          correctedSection = case newCustom.section of
            Nothing -> Nothing
            Just customSection 
              | Just existingSection <- List.findMap (sameSection customSection) allExisting -> Just existingSection
              | otherwise -> Just customSection

          sameSection _custSection {section: Nothing} = Nothing
          sameSection custSection {section: Just existingSection} 
            | (eq `on` CaseInsensitiveString) existingSection custSection = Just existingSection
            | otherwise = Nothing

          allExisting = existingItems <> existingCustom

  rtr Get (Right ResetRecipe) = do
    state <- getState
    setState $ state { cookingState = Nothing }
    HTTPure.noContent

  rtr Post (Right SetRecipeStatus) = go
    where 
      go 
        | Right json <- Json.jsonParser body 
        , Right submittedItem <- decode recipeStepCodec json = do
          state <- getState
          case state.cookingState of
            Nothing -> HTTPure.conflict "The application is not in a state such that modifying recipe steps is possible."
            Just cookingState@{steps} ->
              if not $ List.any (\s -> s.ordinal == submittedItem.ordinal) steps
              then HTTPure.badRequest (i"Could not find step with ordinal: "(show submittedItem.ordinal) :: String)
              else do
                setState $ state { cookingState = Just $ cookingState { steps = replaceStep submittedItem steps } }
                HTTPure.noContent

        | otherwise = HTTPure.badRequest (i"Could not parse request body: "body :: String)

      replaceStep newStep steps = 
        steps <#> \step -> if step.ordinal == newStep.ordinal then newStep else step

  rtr Get (Right RecipesWithSteps) = do
    recipes <- getRecipesWithSteps 
    HTTPure.ok $ Json.stringify $ encode (Codec.array Codec.string) recipes

  rtr Post (Right SelectRecipe) = do
    state <- getState
    steps <- getSteps body
    setState $ state { useCase = Cooking, cookingState = Just steps }
    HTTPure.noContent

  rtr Post (Right SetUseCase) = go
    where 
      go
        | Right json <- Json.jsonParser body 
        , Right submittedItem <- decode useCaseCodec json = do
          state <- getState
          setState $ state { useCase = submittedItem } 
          HTTPure.noContent
        
        | otherwise = HTTPure.badRequest (i"Could not parse request body: "body :: String)

  rtr _ _ = HTTPure.notFound


serveHtml :: ∀ m. MonadAff m => String -> m HTTPure.Response
serveHtml = serveStaticContent "text/html"
serveCss :: ∀ m. MonadAff m => String -> m HTTPure.Response
serveCss = serveStaticContent "text/css"
serveJavascript :: ∀ m. MonadAff m => String -> m HTTPure.Response
serveJavascript = serveStaticContent "text/javascript"

serveStaticContent :: ∀ m. MonadAff m => String -> String -> m HTTPure.Response
serveStaticContent contentType filePath = do
  contents <- liftAff $ readTextFile UTF8 filePath
  HTTPure.ok' (HTTPure.header "Content-Type" (i contentType"; charset=UTF-8")) contents

