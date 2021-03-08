const path = require('path');
const {merge} = require('webpack-merge');
const baseConfig = require('./webpack.config.js');

module.exports = merge(baseConfig, {
  output: {
    path: path.resolve(__dirname, 'release/dist'),
  },
  mode: 'production'
});
