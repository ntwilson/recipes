{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "recipes"
, dependencies =
  [ "aff-promise"
  , "affjax"
  , "argonaut"
  , "codec-argonaut"
  , "concur-react"
  , "console"
  , "debug"
  , "dotenv"
  , "effect"
  , "httpure"
  , "interpolate"
  , "node-fs"
  , "node-process"
  , "option"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "spec"
  , "spec-discovery"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
