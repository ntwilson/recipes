module Recipes.RecipesToIngredients.Spec where

import Backend.Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (cons')
import Recipes.RecipesToIngredients (aggregateGroup, recipesToIngredients)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

infixr 5 cons' as :|

spec :: Spec Unit
spec = do
  describe "aggregateGroup" do
    it "combines same unit elements, and separates different units" do
      let
        input = 
          (  { quantity: 1.8, units: Just "cups" }
          :| { quantity: 6.0, units: Just "ounces" }
          :  { quantity: 2.2, units: Just "cups" }
          : Nil)

      aggregateGroup input `shouldEqual` "4.0 cups, 6.0 ounces" 

  describe "recipesToIngredients" do
    it "groups same ingredients together and produces a grocery list" do
      let 
        rice = { name: "rice", store: "ALDI", section: Nothing, common: true }
        chicken = { name: "chicken", store: "Costco", section: Just "meat", common: false }
        veggies = { name: "frozen vegetables", store: "ALDI", section: Just "frozen", common: true }
        oil = { name: "olive oil", store: "Costco", section: Nothing, common: true }
        noodles = { name: "noodles", store: "Costco", section: Nothing, common: true }
        marinara = { name: "marinara", store: "ALDI", section: Nothing, common: false }

        allIngredients = (rice : chicken : veggies : oil : noodles : marinara : Nil)

        recipeIngredients = 
          ( {recipe: "fried rice", ingredient: "rice", quantity: 2.0, units: Just "cups"}
          : {recipe: "fried rice", ingredient: "chicken", quantity: 2.0, units: Nothing}
          : {recipe: "fried rice", ingredient: "frozen vegetables", quantity: 1.0, units: Just "bags"}
          : {recipe: "fried rice", ingredient: "olive oil", quantity: 0.25, units: Just "cups"}
          : {recipe: "spaghetti", ingredient: "chicken", quantity: 2.0, units: Nothing}
          : {recipe: "spaghetti", ingredient: "frozen vegetables", quantity: 1.5, units: Just "bags"}
          : {recipe: "spaghetti", ingredient: "noodles", quantity: 2.0, units: Just "cakes"}
          : {recipe: "spaghetti", ingredient: "marinara", quantity: 1.0, units: Just "jars"}
          : {recipe: "spaghetti", ingredient: "olive oil", quantity: 6.0, units: Just "ounces"}
          : {recipe: "lots o' chicken", ingredient: "chicken", quantity: 99.0, units: Nothing}
          : Nil)

        selectedRecipes = ("fried rice" : "spaghetti" : Nil)

        expectedIngredients = 
          ( { ingredient: rice, amount: "2.0 cups" }
          : { ingredient: chicken, amount: "4.0" }
          : { ingredient: veggies, amount: "2.5 bags" }
          : { ingredient: oil, amount: "0.25 cups, 6.0 ounces" }
          : { ingredient: noodles, amount: "2.0 cakes" }
          : { ingredient: marinara, amount: "1.0 jars" }
          : Nil)
          
        actualIngredients = recipesToIngredients recipeIngredients allIngredients selectedRecipes

      List.sort actualIngredients `shouldEqual` List.sort expectedIngredients

      


