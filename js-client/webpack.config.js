const name = 'yami-client';

module.exports = {
  mode: 'development',
  target: 'node', // OK?
  entry: './src/index.js',
  output: {
    path: __dirname + '/dist/',
    filename: name + '.js',
    sourceMapFilename: name + '.map',
    library: 'yami-client',
    libraryTarget: 'umd',
    globalObject: 'this'
  },
  devtool: 'source-map',
  node: {
    process: false
  }
};
