module Recipes.Frontend.RecipeSelection where

import Frontend.Prelude

import Concur.React.Props as Props

recipeList :: ∀ f. Traversable f => f String -> Widget HTML String
recipeList recipes = do
  fold
    ( recipes <#> \recipe -> 
      button 
        [ Props.onClick $> recipe
        , Props.className "recipe-button" 
        ] 
        [text recipe]
    )


recipeSelection :: ∀ f. Traversable f => f String -> Widget HTML String
recipeSelection recipes = do
  fold
    [ h3' [text "Choose a recipe to start cooking"]
    , recipeList recipes
    ]
