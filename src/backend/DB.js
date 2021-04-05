var pg = require('pg');

exports.newClient = function(connInfo) {
  return function () {
    return new pg.Client(connInfo);
  }
}

exports.connect = function(promiseToAff) {
  return function(client) {
    return function () {
      return promiseToAff(client.connect().then(function() { return client; }));
    }
  }
}

exports.disconnect = function(promiseToAff) {
  return function(client) {
    return function () { 
      return promiseToAff(client.end());
    }
  }
}
