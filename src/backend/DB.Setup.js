
export async function createDatabase(client, dbName) { 
  const { database } = await client.databases.createIfNotExists({ id: dbName });
  return database;
}

export async function createContainer(database, containerName, partitionKey) { 
  const { container } = await database.containers.createIfNotExists(
    { id: containerName, partitionKey }
  );

  return container;
}
