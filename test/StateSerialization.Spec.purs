module Recipes.StateSerialization.Spec where

import Backend.Prelude

import Data.List (List(..), (:))
import Recipes.StateSerialization (decodeAppState, encodeAppState)
import Recipes.DataStructures (AppState, CurrentUseCase(..), Ingredient, SerializedAppState, ShoppingState(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "decodeAppState" do
    let
      rice = { name: "rice", store: "ALDI", section: Nothing, common: true }
      chicken = { name: "chicken", store: "Costco", section: Just "meat", common: false }
      veggies = { name: "frozen vegetables", store: "ALDI", section: Just "frozen", common: true }
      oil = { name: "olive oil", store: "Costco", section: Nothing, common: true }
      noodles = { name: "noodles", store: "Costco", section: Nothing, common: true }
      marinara = { name: "marinara", store: "ALDI", section: Nothing, common: false }

      ingredients = (rice : chicken : veggies : oil : noodles : marinara : Nil)

      decode :: List Ingredient -> SerializedAppState -> Either String AppState
      decode = decodeAppState

    it "can decode correctly encoded app states from the database" do
      decode ingredients {name: "input recipes", ingredients: Nothing, recipeSteps: Nothing} 
        `shouldEqual` Right {useCase:Shopping, shoppingState: InputRecipes, cookingState: Nothing}
      decode ingredients {name: "check kitchen", ingredients: Nothing, recipeSteps: Nothing} 
        `shouldEqual` (Right {useCase:Shopping, shoppingState: CheckKitchen Nil, cookingState: Nothing})
      decode ingredients {name: "check kitchen", ingredients: Just "", recipeSteps: Nothing} 
        `shouldEqual` (Right {useCase:Shopping, shoppingState: CheckKitchen Nil, cookingState: Nothing})
      decode ingredients {name: "check kitchen", ingredients: Just "1 cup:rice", recipeSteps: Nothing} 
        `shouldEqual` (Right {useCase:Shopping, shoppingState: CheckKitchen ({ingredient:rice, amount:"1 cup"} : Nil), cookingState: Nothing})
      decode ingredients {name: "check kitchen", ingredients: Just "1 cup:rice;2 cakes:noodles;1:chicken", recipeSteps: Nothing} 
        `shouldEqual` (Right {useCase:Shopping, shoppingState: CheckKitchen ({ingredient:rice, amount:"1 cup"} : {ingredient:noodles, amount:"2 cakes"} : {ingredient:chicken, amount:"1"} : Nil), cookingState: Nothing})
      decode ingredients {name: "buy groceries", ingredients: Just "1 cup:rice;2 cakes:noodles;1:chicken", recipeSteps: Nothing} 
        `shouldEqual` (Right {useCase:Shopping, shoppingState: BuyGroceries ({ingredient:rice, amount:"1 cup"} : {ingredient:noodles, amount:"2 cakes"} : {ingredient:chicken, amount:"1"} : Nil) Nil, cookingState: Nothing})

      decode ingredients {name: "cooking(input recipes)", ingredients: Nothing, recipeSteps: Nothing} 
        `shouldEqual` Right {useCase:Cooking, shoppingState: InputRecipes, cookingState: Nothing}
      decode ingredients {name: "cooking(input recipes)", ingredients: Nothing, recipeSteps: Just "Simple Chili::T&&1&&Brown the beef in a pot;F&&2&&Empty the rest of the ingredients into the pot"} 
        `shouldEqual` Right {useCase:Cooking, shoppingState: InputRecipes, cookingState: Just $  
          { recipe: "Simple Chili", steps: {completed: true, ordinal: 1, description: "Brown the beef in a pot"} :  {completed: false, ordinal: 2, description: "Empty the rest of the ingredients into the pot"} : Nil } }

      decode ingredients {name: "cooking(check kitchen)", ingredients: Just "1 cup:rice", recipeSteps: Just "Simple Chili::T&&1&&Brown the beef in a pot;F&&2&&Empty the rest of the ingredients into the pot"} 
        `shouldEqual` Right {useCase:Cooking, shoppingState: CheckKitchen ({ingredient:rice, amount:"1 cup"} : Nil), cookingState: Just $  
          { recipe: "Simple Chili", steps: {completed: true, ordinal: 1, description: "Brown the beef in a pot"} :  {completed: false, ordinal: 2, description: "Empty the rest of the ingredients into the pot"} : Nil } }

    it "throws errors when there's a problem decoding" do
      decode ingredients {name: "unknown", ingredients: Nothing, recipeSteps: Nothing} `shouldSatisfy` isLeft
      decode ingredients {name: "check kitchen", ingredients: Just "unknown", recipeSteps: Nothing} `shouldSatisfy` isLeft

      decode ingredients {name: "cooking(check kitchen)", ingredients: Nothing, recipeSteps: Just "chili::abc"} `shouldSatisfy` isLeft

    it "can round-trip any app state" do 
      let
        roundTripTest appState = 
          (encodeAppState appState # decode ingredients) `shouldEqual` Right appState

      roundTripTest {useCase:Shopping, shoppingState: InputRecipes, cookingState: Nothing}
      roundTripTest {useCase:Shopping, shoppingState: CheckKitchen ({ingredient:rice, amount:"1 cup"} : {ingredient:chicken, amount:"1"} : {ingredient:noodles, amount:"2 cakes"} : Nil), cookingState: Nothing}
      roundTripTest {useCase:Shopping, shoppingState: CheckKitchen Nil, cookingState: Nothing}
      roundTripTest {useCase:Shopping, shoppingState: BuyGroceries ({ingredient:rice, amount:"1 cup"} : Nil) Nil, cookingState: Nothing}
      roundTripTest 
        { useCase:Shopping, shoppingState: BuyGroceries ({ingredient:rice, amount:"1 cup"} : Nil) Nil
        , cookingState: Just {recipe: "chili", steps: ({completed: true, ordinal: 1, description: "part 1"} : {completed: false, ordinal: 2, description: "part 2"} : Nil)}
        }
