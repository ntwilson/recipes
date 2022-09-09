const path = require('path');

module.exports = {
  entry: './src/backend/entry.js',
  output: {
    path: path.resolve(__dirname, 'release'),
  },
  mode: 'development',
};