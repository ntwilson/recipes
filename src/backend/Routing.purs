module Recipes.Backend.Routing where

import Backend.Prelude

import Data.Argonaut.Core as Json
import Data.Argonaut.Parser as Json
import Data.Codec.Argonaut.Common as Codec
import Data.List (List(..), (:))
import Data.List as List
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import HTTPurple (Method(..))
import HTTPurple as HTTPure
import Node.Encoding (Encoding(..))
import Recipes.API (AddItemValue, RecipeRoute(..), setItemStatusCodec)
import Recipes.Backend.CosmosDB (DELETE_ERROR, printDeleteError, printQueryError)
import Recipes.Backend.LoadState (allIngredients, allRecipeIngredients, allRecipes, getRecipesWithSteps, getState, getSteps, setState)
import Recipes.Backend.RecipesToIngredients (recipesToIngredients)
import Recipes.DataStructures (CurrentUseCase(..), ShoppingState(..), appStateCodec, ingredientCodec, recipeStepCodec, useCaseCodec)


run :: String -> HTTPure.Request RecipeRoute -> HTTPure.ResponseM
run dist rqst = 
  handleErrors {stringError: handler} do
    router dist rqst

  where
  handler err = do
    log (i"-----------\nServer Error: "err"\n-----------")
    HTTPure.internalServerError err


router :: ∀ m. MonadAff m => String -> HTTPure.Request RecipeRoute -> ExceptV (STRING_ERROR ()) m HTTPure.Response
router dist rqst = do
  body <- HTTPure.toString rqst.body
  rtr rqst.method rqst.route body
  where
  rtr :: _ -> _ -> _ -> ExceptV (STRING_ERROR ()) m _
  rtr Get Home _ = serveHtml (i dist"/index.html")
  rtr Get CSS _ = serveCss (i dist"/index.css")
  rtr Get JS _ = serveJavascript (i dist"/main.js")

  rtr Get Recipes _ = printQueryError do
    contents <- encode (Codec.array Codec.string) <$> allRecipes
    HTTPure.ok $ Json.stringify contents

  rtr Get Ingredients _ = printQueryError do
    contents <- encode (Codec.list ingredientCodec) <$> allIngredients
    HTTPure.ok $ Json.stringify contents

  rtr Post SubmitRecipes body = printDeleteError go 
    where 
      go :: ExceptV (DELETE_ERROR + STRING_ERROR ()) m _
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

  rtr Get CurrentState _ = printQueryError do
    state <- getState
    ingredients <- allIngredients
    HTTPure.ok $ Json.stringify $ encode (appStateCodec ingredients) state

  rtr Get ResetState _ = printDeleteError do
    state <- getState
    setState $ state { useCase = Shopping, shoppingState = InputRecipes }
    HTTPure.noContent

  rtr Get SubmitPantry _ = printDeleteError do
    state <- getState
    case state of 
      {useCase: Shopping, shoppingState: CheckKitchen items} -> do
        setState $ state {shoppingState = BuyGroceries items Nil}
        HTTPure.noContent
      _ -> HTTPure.conflict "No items can or will exist until recipes are input."

  rtr Post SetItemStatus body = printDeleteError go
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

  rtr Post AddItem body = printDeleteError go 
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

  rtr Get ResetRecipe _ = printDeleteError do
    state <- getState
    setState $ state { cookingState = Nothing }
    HTTPure.noContent

  rtr Post SetRecipeStatus body = printDeleteError go
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

  rtr Get RecipesWithSteps _ = printQueryError do
    recipes <- getRecipesWithSteps 
    HTTPure.ok $ Json.stringify $ encode (Codec.array Codec.string) recipes

  rtr Post SelectRecipe body = printDeleteError do
    state <- getState
    steps <- getSteps body
    setState $ state { useCase = Cooking, cookingState = Just steps }
    HTTPure.noContent

  rtr Post SetUseCase body = printDeleteError go
    where 
      go
        | Right json <- Json.jsonParser body 
        , Right submittedItem <- decode useCaseCodec json = do
          state <- getState
          setState $ state { useCase = submittedItem } 
          HTTPure.noContent
        
        | otherwise = HTTPure.badRequest (i"Could not parse request body: "body :: String)

  rtr _ _ _ = HTTPure.notFound


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

