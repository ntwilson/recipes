module Recipes.Frontend.RecipeSelection where

import Frontend.Prelude

import Concur.React.Props as Props
import Data.List (List)

recipeSelection :: List String -> Widget HTML String
recipeSelection recipes = do
  fold
    ( recipes <#> \recipe -> 
      button 
        [ Props.onClick $> recipe
        , Props.style 
          { marginTop: "1em", marginBottom: "1em", border: "none", backgroundColor: "white" }
        ] 
        [text recipe]
    )
