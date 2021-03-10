# recipes
Track recipes and build grocery lists.

### Setup
This app is meant to be deployed to Heroku, and the purescript/spago installer doesn't work on Heroku, sooooo... for setup, please run the setup script in addition to `npm install`, which installs all the purescript dependencies you need locally.  This is needed for several of the npm scripts to run. 

This repo needs to include a bundled version of the latest release, which is is the "release" folder, generated via `npm run release`.  For normal develoment, please use `npm run` `ps`/`go`/`watch`/`pack`/`test` 

For local development, you will need a Postgres database to connect to.  After setting up postgres and a local database to use, run DB/schema.sql to add the necessary tables to your database, and DB/staticData.sql to populate the database.

You will need to setup a local .env file to connect to the database and setup other server configurations.  See sample.env for how to construct your own .env file
