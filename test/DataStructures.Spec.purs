module Recipes.DataStructures.Spec where

import Backend.Prelude

import Data.List (List(..), (:))
import Recipes.DataStructures (AppState(..), Ingredient, SerializedAppState, decodeAppState, encodeAppState)
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
      decode ingredients {name: "input recipes", ingredients: Nothing} `shouldEqual` Right InputRecipes
      decode ingredients {name: "check kitchen", ingredients: Nothing} `shouldEqual` (Right $ CheckKitchen Nil)
      decode ingredients {name: "check kitchen", ingredients: Just ""} `shouldEqual` (Right $ CheckKitchen Nil)
      decode ingredients {name: "check kitchen", ingredients: Just "1 cup:rice"} `shouldEqual` (Right $ CheckKitchen ({ingredient:rice, amount:"1 cup"} : Nil))
      decode ingredients {name: "check kitchen", ingredients: Just "1 cup:rice;2 cakes:noodles;1:chicken"} 
        `shouldEqual` (Right $ CheckKitchen ({ingredient:rice, amount:"1 cup"} : {ingredient:noodles, amount:"2 cakes"} : {ingredient:chicken, amount:"1"} : Nil))
      decode ingredients {name: "buy groceries", ingredients: Just "1 cup:rice;2 cakes:noodles;1:chicken"} 
        `shouldEqual` (Right $ BuyGroceries ({ingredient:rice, amount:"1 cup"} : {ingredient:noodles, amount:"2 cakes"} : {ingredient:chicken, amount:"1"} : Nil) Nil)

    it "throws errors when there's a problem decoding" do
      decode ingredients {name: "unknown", ingredients: Nothing} `shouldSatisfy` isLeft
      decode ingredients {name: "check kitchen", ingredients: Just "unknown"} `shouldSatisfy` isLeft

    it "can round-trip any app state" do 
      let
        roundTripTest appState = 
          (encodeAppState appState # decode ingredients) `shouldEqual` Right appState

      roundTripTest InputRecipes 
      roundTripTest (CheckKitchen ({ingredient:rice, amount:"1 cup"} : {ingredient:chicken, amount:"1"} : {ingredient:noodles, amount:"2 cakes"} : Nil))
      roundTripTest (CheckKitchen Nil)
      roundTripTest (BuyGroceries ({ingredient:rice, amount:"1 cup"} : Nil) Nil)
