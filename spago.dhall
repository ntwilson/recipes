{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "recipes"
, dependencies =
  [ "aff-promise"
  , "affjax"
  , "argonaut"
  , "concur-react"
  , "console"
  , "debug"
  , "dotenv"
  , "effect"
  , "foreign-generic"
  , "httpure"
  , "interpolate"
  , "node-fs"
  , "node-postgres"
  , "node-process"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
