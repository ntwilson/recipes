module Recipes.Frontend.Main where

import Frontend.Prelude

import Affjax (defaultRequest, printError, request)
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Data.Argonaut (decodeJson, printJsonDecodeError)
import Effect.Aff.Class (liftAff)
import Recipes.API (TestValue, RecipesValue, recipesRoute, routeStr, testRoute)

loadTest :: Aff String
loadTest = do
  resp <- request (defaultRequest { url = routeStr testRoute, responseFormat = ResponseFormat.json })
  pure $ either (\err -> i "Error!  "err) identity $ processedResp resp

  where 
    processedResp resp = do
      {body} <- resp # lmap printError
      (x :: TestValue) <- decodeJson body # lmap printJsonDecodeError
      pure $ show x

loadRecipes :: Aff RecipesValue
loadRecipes = do
  resp <- request (defaultRequest { url = routeStr recipesRoute, responseFormat = ResponseFormat.json })
  {body} <- resp # lmap printError # liftError
  decodeJson body # lmap printJsonDecodeError # liftError


content :: Widget HTML Void
content = do
  recipes <- (text "Loading..." <|> liftAff loadRecipes)
  fold (recipes <#> \recipe -> div' [input [Props._type "checkbox"] <|> text recipe])

main :: Effect Unit
main = runWidgetInDom "contents" content
