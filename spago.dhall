{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "recipes"
, dependencies =
  [ "concur-react"
  , "console"
  , "dotenv"
  , "effect"
  , "httpure"
  , "interpolate"
  , "node-fs"
  , "node-process"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
