const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  devtool: 'source-map',
  entry: {
    all: ['@babel/polyfill', __dirname + '/docs_src/js/index'],
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
      template: __dirname + '/docs_src/html/index.html',
    }),
  ],
  devServer: {
    port: 3000,
    inline: true,
    stats: 'minimal',
  },
}
