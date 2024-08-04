# recipes
Track recipes and build grocery lists.

### Setup
This app is meant to be deployed as an Azure WebApp, which is done via the deployment GitHub Action, automatically when anything is merged into main. 

For authentication, you will need to use azure identity. Typically that means that you will locally need to run `az login` before you will be able to connect to the database. Also, permissions to use the database cannot be granted through the portal, and can only be granted via the command:

`az cosmosdb sql role assignment create --account-name 'ntw-cosmos' --resource-group 'nutriments' --scope "/" --principal-id $principalId --role-definition-id 00000000-0000-0000-0000-000000000002`

You will need to setup a local .env file to connect to the database and setup other server configurations. You may just copy sample.env for all the default values

