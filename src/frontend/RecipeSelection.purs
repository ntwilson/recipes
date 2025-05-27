module Recipes.Frontend.RecipeSelection where

import Frontend.Prelude

import Concur.React.Props as Props

recipeList :: ∀ f. Traversable f => f String -> Widget HTML String
recipeList recipes = 
  fold
    ( recipes <#> \recipe -> 
      button 
        [ Props.onClick $> recipe
        , Props.className "recipe-button" 
        ] 
        [text recipe]
    )


recipeSelection :: ∀ w f. LiftWidget HTML w => Traversable f => f String -> w String
recipeSelection recipes = liftWidget $
  fold
    [ h3' [text "Choose a recipe to start cooking"]
    , recipeList recipes
    ]
