import babel from 'rollup-plugin-babel'

export default {
  input: 'src/index.js',
  output: {
    file: 'lib/processes.js',
    format: 'cjs',
    sourceMap: 'inline',
  },
  plugins: [
    babel({
      babelrc: false,
    }),
  ],
}
