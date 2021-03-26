module Recipes.Frontend.Main where

import Frontend.Prelude

import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Control.Monad.Except.Checked (safe)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError)
import Data.List (List(..), (:))
import Data.List as List
import Data.Variant (SProxy(..), Variant, inj)
import Recipes.API (TestValue, RecipesValue, recipesRoute, routeStr, testRoute)

type JsonError r = (jsonError :: JsonDecodeError | r)
jsonError :: forall r. JsonDecodeError -> Variant (JsonError r)
jsonError = inj (SProxy :: _ "jsonError") 

type HttpError r = (httpError :: Affjax.Error | r)
httpError :: forall r. Affjax.Error -> Variant (HttpError r)
httpError = inj (SProxy :: _ "httpError") 

request :: ∀ a e. Affjax.Request a → ExceptV (HttpError + e) Aff (Affjax.Response a)
request req = do
  maybeResp <- lift $ Affjax.request req
  except $ lmap httpError maybeResp

decode :: ∀ a e m. Monad m => DecodeJson a ⇒ Json → ExceptV (JsonError + e) m a
decode json = 
  case decodeJson json of 
    Left err -> throwError $ jsonError err
    Right ans -> pure ans

loadTest :: forall r. ExceptV (HttpError + JsonError + r) Aff String
loadTest = do
  {body} <- request (Affjax.defaultRequest { url = routeStr testRoute, responseFormat = ResponseFormat.json })
  (x :: TestValue) <- decode body 
  pure $ show x

tryLoadRecipes :: forall r. ExceptV (HttpError + JsonError + r) Aff RecipesValue
tryLoadRecipes = do
  {body} <- request (Affjax.defaultRequest { url = routeStr recipesRoute, responseFormat = ResponseFormat.json })
  decode body 

loadRecipes :: Widget HTML (Array String)
loadRecipes = do
  maybeRecipes <-
    tryLoadRecipes <#> Right # handleError
      { httpError: \err -> pure $ Left $ Affjax.printError err
      , jsonError: \err -> pure $ Left $ show err
      }
    # safe
    # liftAff

  case maybeRecipes of 
    Right recipes -> pure recipes
    Left err -> text $ i"Failed to load recipes. "err


recipeList :: forall f. Traversable f => f String -> Widget HTML String
recipeList allRecipes = 
  fold (allRecipes <#> \recipe -> div' [input [Props._type "checkbox", Props.onChange $> recipe] <|> text recipe])

data RecipeSelection = AnotherSelection String | Submit
selectedRecipes :: forall f. Traversable f => f String -> List String -> Widget HTML $ List String
selectedRecipes allRecipes selectedRecipesSoFar = do
  selection <- 
    fold
      [ recipeList allRecipes <#> AnotherSelection
      , div' [button [Props.onClick] [text "Submit"]] $> Submit
      ]

  case selection of 
    Submit -> pure selectedRecipesSoFar
    AnotherSelection recipe -> selectedRecipes allRecipes $ updateSelection recipe

  where 
    updateSelection nextSelected = 
      if nextSelected `elem` selectedRecipesSoFar
      then List.delete nextSelected selectedRecipesSoFar
      else nextSelected : selectedRecipesSoFar

content :: Widget HTML Void
content = do
  recipes <- (text "Loading..." <|> loadRecipes)
  selected <- selectedRecipes recipes Nil
  text $ i"You selected: "(show selected)

main :: Effect Unit
main = runWidgetInDom "contents" content
