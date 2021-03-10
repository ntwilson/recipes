var pg = require('pg');

exports.newClient = function(connInfo) {
  return function () {
    return new pg.Client(connInfo);
  }
}

exports.connect = function(client) {
  return function () {
    client.connect();
    return client;
  }
}