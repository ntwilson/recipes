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
  , "dotenv"
  , "effect"
  , "httpure"
  , "interpolate"
  , "node-fs"
  , "node-process"
  , "postgresql-client"
  , "psci-support"
  , "selda"
  , "spec"
  , "spec-discovery"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
