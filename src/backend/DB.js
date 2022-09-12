import { CosmosClient } from "@azure/cosmos";

export function cosmosClient(config) { return new CosmosClient(config); }

export function database({ endpoint, key, databaseId }) {
  return new CosmosClient({endpoint, key}).database(databaseId);
}

export function getContainerImpl(database, containerId) {
  return database.container(containerId);
}

// // https://docs.microsoft.com/en-us/azure/cosmos-db/sql/sql-api-nodejs-get-started?tabs=linux
export async function queryImpl(container, query, parameters) {
  const {resources} = await container.items.query({ query, parameters }).fetchAll();
  return resources;
}

export async function readAllImpl(container) {
  const {resources} = await container.items.readAll().fetchAll(); 
  return resources;
}

export async function insertImpl(container, item) { 
  const { resource } = await container.items.create(item);
  return resource;
}

export async function updateImpl(container, item) { 
  const { resource } = await container.item(item.id, item.category).replace(item);
  return resource;
}

export async function getItemImpl(container, id, partitionKey) { 
  const {body} = await container.item(id, partitionKey).read();
  return body;
}

export async function deleteImpl(container, id, partitionKey) {
  const { resource } = await container.item(id, partitionKey).delete();
  return resource;
}

export function container(database, containerName) {
  return database.container(containerName);
}

