# recipes
Track recipes and build grocery lists.

### Setup
This app is meant to be deployed to Heroku, and the purescript/spago installer doesn't work on Heroko, sooooo...

For setup, run `npm i purescript@0.13.8 purescript-psa spago@0.19.1`

This repo needs to include a bundled version of the latest release, which is is the "release" folder, generated via `npm run release`.  For normal develoment, please use `npm run` `build`/`go`/`watch`/`pack`/`test` 

`npm run go` only works on windows.  For linux, use `npm run go-sh`
