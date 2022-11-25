
// We would like to create our database programmatically, but the JS CosmosDB Client won't allow for creating an 
// autoscale database, which then would limit you to 2 containers (with a minimum of 400 RU/s per container, and 
// a maximum of 1,000 RU/s per account on the free tier)

// export async function createDatabase(client, dbName) { 
//   const { database } = await client.databases.createIfNotExists({ id: dbName });
//   return database;
// }

export async function createContainer(database, containerName, partitionKey) { 
  const { container } = await database.containers.createIfNotExists(
    { id: containerName, partitionKey }
  );

  return container;
}

export const deleteContainer = (database) => (containerName) => async () => {
  await database.container(containerName).delete();
}
