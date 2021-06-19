var pg = require('pg');

exports.newClient = function(connInfo) {
  return function () {
    return new pg.Client(connInfo);
  }
}

exports.connect = function(client) {
  return function () {
    return client.connect().then(function() { return client; });
  }
}

exports.disconnect = function(client) {
  return function () { 
    return client.end();
  }
}

exports.unsafeStringify = function(a) { return JSON.stringify(a); }

