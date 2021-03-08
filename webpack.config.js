const path = require('path');

module.exports = {
  entry: './output/Recipes.Frontend.Main/index.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
  },
  mode: 'production',
};