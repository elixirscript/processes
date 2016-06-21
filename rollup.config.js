import { rollup } from 'rollup';
import babel from 'rollup-plugin-babel';
import nodeResolve from 'rollup-plugin-node-resolve';

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
