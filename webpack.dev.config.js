const HtmlWebpackPlugin = require('html-webpack-plugin')
const path = require('path')

module.exports = {
  devtool: 'source-map',
  entry: {
    all: ['@babel/polyfill', __dirname + '/src/docs/js/index.js'],
  },
  output: {
    path: __dirname + '/docs',
    filename: '[name].js',
    publicPath: '/',
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: [
          {loader: 'style-loader', options: {sourceMap: true}},
          {loader: 'css-loader', options: {sourceMap: true}},
        ],
      },
      {
        test: /\.js$/,
        exclude: /(node_modules)/,
        loader: 'babel-loader',
      },
    ],
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: __dirname + '/src/docs/html/index.html',
    }),
  ],
  devServer: {
    port: 3000,
    inline: true,
    stats: 'minimal',
  },
}
