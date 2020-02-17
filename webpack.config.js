const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = (_, config) => {
  const isProd = config.mode === 'production'
  return {
    output: {
      publicPath: '/',
    },
    resolve: {
      extensions: ['.js', '.elm'],
    },
    plugins: [new HtmlWebpackPlugin({ template: 'src/index.html' })],
    module: {
      rules: [
        {
          include: /\.elm/,
          use: [
            //'elm-hot-webpack-loader',
            { loader: 'elm-webpack-loader', options: { optimize: isProd , debug: !isProd} },
          ],
        },
        { include: /\.css/, use: ['style-loader', 'css-loader'] },
      ],
    },
    devServer: {
      historyApiFallback: true,
      hot: true,
      overlay: true,
    },
  }
}
