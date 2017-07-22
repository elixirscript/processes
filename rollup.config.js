import babel from 'rollup-plugin-babel';

export default {
  entry: 'src/index.js',
  dest: 'lib/processes.js',
  sourceMap: 'inline',
  format: 'cjs',
  plugins: [
    babel({
      babelrc: false
    })
  ]
};
