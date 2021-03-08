module Recipes.Frontend.Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)

content :: Widget HTML Void
content = text "Hello World!"

main :: Effect Unit
main = runWidgetInDom "contents" content
