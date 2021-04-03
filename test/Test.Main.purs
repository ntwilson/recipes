module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (message)
import Effect.Class.Console (log)
import Recipes.ErrorHandling (launchAffWithHandler)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAffWithHandler (\e -> log ("couldn't start test run. " <> message e)) do
  specs <- discover ".*\\.Spec"
  runSpec [consoleReporter] specs
