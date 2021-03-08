const path = require('path');

module.exports = {
  entry: './src/frontend/entry.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
  },
  mode: 'development',
};