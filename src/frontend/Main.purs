module Recipes.Frontend.Main where

import Frontend.Prelude

import Affjax (defaultRequest, printError, request)
import Affjax.ResponseFormat as ResponseFormat
import Concur.React.Run (runWidgetInDom)
import Effect.Aff.Class (liftAff)

loadContents :: Aff String
loadContents = do
  resp <- request (defaultRequest { url = "/api/test", responseFormat = ResponseFormat.string })
  pure $ case resp of 
    Left err -> i "Error!  "(printError err)
    Right x -> show x

content :: Widget HTML Void
content = do
  msg <- liftAff loadContents
  text msg

main :: Effect Unit
main = runWidgetInDom "contents" content
