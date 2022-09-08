module Recipes.Backend.Routing where

import Backend.Prelude

import Control.Monad.Except (runExceptT)
import Data.Argonaut as Json
import Data.List (List(..), (:))
import Data.List as List
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import HTTPure (Method(..))
import HTTPure as HTTPure
import HTTPure.Body as Body
import Node.Encoding (Encoding(..))
import Recipes.API (AddItemValue, RecipeRoute(..), SetItemStatusValue, SetRecipeStepStatusValue, SetUseCaseValue, recipeRouteDuplex, routeStr)
import Recipes.Backend.LoadState (allIngredients, allRecipeIngredients, allRecipes, getRecipesWithSteps, getSerializedState, getState, getSteps, setState)
import Recipes.Backend.RecipesToIngredients (recipesToIngredients)
import Recipes.DataStructures (CurrentUseCase(..), ShoppingState(..))
import Routing.Duplex as Routing

router :: String -> HTTPure.Request -> HTTPure.ResponseM
router dist rqst = do
  r <- runExceptT do
    body <- lift $ Body.toString rqst.body
    let
      result :: ExceptT String Aff HTTPure.Response
      result = rtr rqst.method $ Routing.parse recipeRouteDuplex $ routeStr rqst.path
        where
        rtr Get (Right Home) = lift $ serveHtml (i dist"/index.html")
        rtr Get (Right CSS) = lift $ serveCss (i dist"/index.css")
        rtr Get (Right JS) = lift $ serveJavascript (i dist"/main.js")

        rtr Get (Right Recipes) = do
          contents <- encodeJson <$> allRecipes
          HTTPure.ok $ Json.stringify contents

        rtr Get (Right Ingredients) = do
          contents <- encodeJson <$> allIngredients
          HTTPure.ok $ Json.stringify contents

        rtr Post (Right SubmitRecipes) = go 
          where 
            go
              | Right json <- Json.parseJson body
              , Right (submittedRecipes :: List String) <- decodeJson json = do
                pairings <- allRecipeIngredients
                ingredients <- allIngredients
                state <- getState
                let groceryList = recipesToIngredients pairings ingredients submittedRecipes
                setState { useCase: Shopping, shoppingState: CheckKitchen groceryList, cookingState: state.cookingState }
                HTTPure.noContent
              
              | otherwise = HTTPure.badRequest "Could not parse request body"

        rtr Get (Right CurrentState) = do
          state <- getSerializedState
          HTTPure.ok $ Json.stringify $ encodeJson state

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
              | Right json <- Json.parseJson body
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

        rtr Post (Right AddItem) = go 
          where 
            go 
              | Right json <- Json.parseJson body 
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

                sameSection _custSection {section: Nothing} = Nothing
                sameSection custSection {section: Just existingSection} 
                  | equating CaseInsensitiveString existingSection custSection = Just existingSection
                  | otherwise = Nothing

                allExisting = existingItems <> existingCustom

        rtr Get (Right ResetRecipe) = do
          state <- getState
          setState $ state { cookingState = Nothing }
          HTTPure.noContent

        rtr Post (Right SetRecipeStatus) = go
          where 
            go 
              | Right json <- Json.parseJson body 
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

              | otherwise = HTTPure.badRequest (i"Could not parse request body: "body :: String)

            replaceStep newStep steps = 
              steps <#> \step -> if step.ordinal == newStep.ordinal then newStep else step

        rtr Get (Right RecipesWithSteps) = do
          recipes <- getRecipesWithSteps 
          HTTPure.ok $ Json.stringify $ encodeJson recipes

        rtr Post (Right SelectRecipe) = do
          state <- getState
          steps <- getSteps body
          setState $ state { useCase = Cooking, cookingState = Just steps }
          HTTPure.noContent

        rtr Post (Right SetUseCase) = go
          where 
            go
              | Right json <- Json.parseJson body 
              , Right (submittedItem :: SetUseCaseValue) <- decodeJson json = do
                state <- getState
                setState $ state { useCase = submittedItem } 
                HTTPure.noContent
              
              | otherwise = HTTPure.badRequest (i"Could not parse request body: "body :: String)

        rtr _ _ = HTTPure.notFound

    result

  case r of
    Left err -> do
      log (i"-----------\nServer Error: "(show err)"\n-----------")
      HTTPure.internalServerError $ show err

    Right handler -> 
      pure handler

serveHtml :: String -> HTTPure.ResponseM
serveHtml = serveStaticContent "text/html"
serveCss :: String -> HTTPure.ResponseM
serveCss = serveStaticContent "text/css"
serveJavascript :: String -> HTTPure.ResponseM
serveJavascript = serveStaticContent "text/javascript"

serveStaticContent :: String -> String -> HTTPure.ResponseM
serveStaticContent contentType filePath = do
  contents <- readTextFile UTF8 filePath
  HTTPure.ok' (HTTPure.header "Content-Type" (i contentType"; charset=UTF-8")) contents

