import eslint from '@eslint/js';
import tseslint from 'typescript-eslint';
import prettierConfig from 'eslint-config-prettier';

// prettier-ignore
export default tseslint.config(
    eslint.configs.recommended,
    tseslint.configs.recommended,
    prettierConfig,
);

// module.exports = {
//   env: {
//     node: true,
//   },
//   parser: '@typescript-eslint/parser',
//   extends: ['plugin:@typescript-eslint/recommended', 'plugin:prettier/recommended'],
//   rules: {
//     '@typescript-eslint/ban-ts-comment': 'off',
//     '@typescript-eslint/no-explicit-any': 'off',
//     '@typescript-eslint/no-non-null-assertion': 'off',
//     '@typescript-eslint/no-namespace': 'off',
//     '@typescript-eslint/no-empty-function': 'off',
//     '@typescript-eslint/explicit-function-return-type': 'off',
//     '@typescript-eslint/explicit-module-boundary-types': 'off',
//   },
// };
