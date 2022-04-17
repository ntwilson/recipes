module Recipes.Frontend.RecipeStepList where

import Frontend.Prelude

import Affjax.RequestBody (RequestBody(..))
import Concur.React.Props as Props
import Data.HTTP.Method (Method(..))
import Recipes.API (SetRecipeStepStatusValue)
import Recipes.API as Routing
import Recipes.DataStructures (RecipeStep, CookingState)
import Recipes.Frontend.Http (expectRequest)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

data RecipeStepListAction = CheckStep SetRecipeStepStatusValue | ResetRecipe 
recipeStepList :: CookingState -> Widget HTML Unit
recipeStepList state@{recipe, steps} = do
  action <- 
    h3' [text recipe]
    <|> 
    (fold (recipeStepListItem <$> steps) <#> CheckStep)
    <|> 
    button [Props.onClick $> ResetRecipe, Props.style {marginTop: "1em"}] [text "Finished cooking"]


  case action of 
    CheckStep listItemToSubmit -> do
      liftEffect $ checkStep listItemToSubmit
      let updatedSteps = steps <#> \oldStep -> if oldStep.ordinal == listItemToSubmit.ordinal then listItemToSubmit else oldStep
      recipeStepList $ state { steps = updatedSteps }

    ResetRecipe -> do
      liftAff $ resetRecipe 
      liftEffect (window >>= location >>= reload)

  where
    checkStep step = launchAff_ $ expectRequest $ defaultRequest 
      { method = Left POST, url = Routing.print Routing.SetRecipeStatus
      , content = Just $ Json $ encodeJson step
      }

    resetRecipe = expectRequest $ defaultRequest { method = Left GET, url = Routing.print Routing.ResetRecipe }


recipeStepListItem :: RecipeStep -> Widget HTML SetRecipeStepStatusValue
recipeStepListItem step = do
  div [Props.className "checkbox-container"] 
    [ input [Props._type "checkbox", void Props.onChange, Props.checked step.completed, Props.className "multiline"] 
    , span [void Props.onClick, Props.className "checkbox-text"] [text step.description]
    ]

  pure $ step { completed = not step.completed }