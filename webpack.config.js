"use strict";

var webpack = require("webpack");

module.exports = {
  context: __dirname + "/src",
  devtool: "eval",
  entry: [
    "webpack/hot/only-dev-server",
    "webpack-hot-middleware/client",
    "./index"
  ],
  output: {
    path: __dirname + "/dist",
    filename: "bundle.js",
    publicPath: "/"
  },
  resolve: {
    extensions: ["", ".js"]
  },
  resolveLoader: {
    root: __dirname + "/node_modules"
  },
  module: {
    loaders: [{
      include: __dirname + "/src",
      test: /\.js$/,
      loader: "react-hot"
    }, {
      include: __dirname + "/src",
      test: /\.css$/,
      loader: "style-loader!css-loader"
    }]
  },
  plugins: [
    new webpack.optimize.OccurenceOrderPlugin(),
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoErrorsPlugin()
  ]
};