{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "recipes"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "checked-exceptions"
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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
