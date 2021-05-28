module Recipes.Backend.Routing where

import Backend.Prelude

import Data.Argonaut as Json
import Data.List (List(..), (:))
import Data.List as List
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import HTTPure (Method(..))
import HTTPure as HTTPure
import Node.Encoding (Encoding(..))
import Recipes.API (AddItemValue, SetItemStatusValue, SetRecipeStepStatusValue, addItemRoute, currentStateRoute, ingredientsRoute, recipesRoute, resetRecipeRoute, resetStateRoute, setItemStatusRoute, setRecipeStepStatusRoute, submitPantryRoute, submitRecipesRoute)
import Recipes.Backend.LoadState (allIngredients, allRecipeIngredients, allRecipes, getSerializedState, getState, setState)
import Recipes.Backend.RecipesToIngredients (recipesToIngredients)
import Recipes.DataStructures (CurrentUseCase(..), ShoppingState(..))

router :: String -> HTTPure.Request -> HTTPure.ResponseM
router dist rqst = 
  catchError (rtr rqst.method rqst.path) errHandler
  where
    rtr Get [] = serveHtml (i dist"/index.html")
    rtr Get ["main.js"] = serveJavascript (i dist"/main.js")

    rtr Get route | route == recipesRoute = do
      contents <- encodeJson <$> allRecipes 
      HTTPure.ok $ Json.stringify contents

    rtr Get route | route == ingredientsRoute = do
      contents <- encodeJson <$> allIngredients
      HTTPure.ok $ Json.stringify contents

    rtr Post route | route == submitRecipesRoute = go 
      where 
        go
          | Right json <- Json.parseJson rqst.body
          , Right (submittedRecipes :: List String) <- decodeJson json = do
            pairings <- allRecipeIngredients
            ingredients <- allIngredients
            state <- getState
            let groceryList = recipesToIngredients pairings ingredients submittedRecipes
            setState { useCase: Shopping, shoppingState: CheckKitchen groceryList, cookingState: state.cookingState }
            HTTPure.noContent
          
          | otherwise = HTTPure.badRequest "Could not parse request body"

    rtr Get route | route == currentStateRoute = do
      state <- getSerializedState
      HTTPure.ok $ Json.stringify $ encodeJson state

    rtr Get route | route == resetStateRoute = do
      state <- getState
      setState $ state { useCase = Shopping, shoppingState = InputRecipes }
      HTTPure.noContent

    rtr Get route | route == submitPantryRoute = do
      state <- getState
      case state of 
        {useCase: Shopping, shoppingState: CheckKitchen items} -> do
          setState $ state {shoppingState = BuyGroceries items Nil}
          HTTPure.noContent
        _ -> HTTPure.conflict "No items can or will exist until recipes are input."

    rtr Post route | route == setItemStatusRoute = go
      where
        go 
          | Right json <- Json.parseJson rqst.body
          , Right (submittedItem :: SetItemStatusValue) <- decodeJson json = do
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

    rtr Post route | route == addItemRoute = go 
      where 
        go 
          | Right json <- Json.parseJson rqst.body 
          , Right (submittedItem :: AddItemValue) <- decodeJson json = do
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
            correctedStore = case List.find (\{store: existingStore} -> equating CaseInsensitiveString existingStore newCustom.store) allExisting of
              Just existingItem -> existingItem.store
              Nothing -> newCustom.store

            correctedSection = case newCustom.section of
              Nothing -> Nothing
              Just customSection 
                | Just existingSection <- List.findMap (sameSection customSection) allExisting -> Just existingSection
                | otherwise -> Just customSection

            sameSection custSection {section: Nothing} = Nothing
            sameSection custSection {section: Just existingSection} 
              | equating CaseInsensitiveString existingSection custSection = Just existingSection
              | otherwise = Nothing

            allExisting = existingItems <> existingCustom

    rtr Get route | route == resetRecipeRoute = do
      state <- getState
      setState $ state { cookingState = Nothing }
      HTTPure.noContent

    rtr Post route | route == setRecipeStepStatusRoute = go
      where 
        go 
          | Right json <- Json.parseJson rqst.body 
          , Right (submittedItem :: SetRecipeStepStatusValue) <- decodeJson json = do
            state <- getState
            case state.cookingState of
              Nothing -> HTTPure.conflict "The application is not in a state such that modifying recipe steps is possible."
              Just cookingState@{steps} ->
                if not $ List.any (\s -> s.ordinal == submittedItem.ordinal) steps
                then HTTPure.badRequest (i"Could not find step with ordinal: "(show submittedItem.ordinal) :: String)
                else do
                  setState $ state { cookingState = Just $ cookingState { steps = replaceStep submittedItem steps } }
                  HTTPure.noContent

          | otherwise = HTTPure.badRequest (i"Could not parse request body: "rqst.body :: String)

        replaceStep newStep steps = 
          steps <#> \step -> if step.ordinal == newStep.ordinal then newStep else step

    rtr _ _ = HTTPure.notFound
    
    errHandler err = HTTPure.internalServerError $ show err

serveHtml :: String -> HTTPure.ResponseM
serveHtml = serveStaticContent "text/html"
serveJavascript :: String -> HTTPure.ResponseM
serveJavascript = serveStaticContent "text/javascript"

serveStaticContent :: String -> String -> HTTPure.ResponseM
serveStaticContent contentType filePath = do
  contents <- readTextFile UTF8 filePath
  HTTPure.ok' (HTTPure.header "Content-Type" (i contentType"; charset=UTF-8")) contents

