const js = require("@eslint/js")
const react = require("eslint-plugin-react")
const globals = require("globals")

module.exports = [
  js.configs.recommended,
  react.configs.flat.recommended,
  {
    languageOptions: {
      globals: {
        ...globals.browser,
        __PATH_PREFIX__: "readonly",
      },
      parserOptions: {
        ecmaFeatures: {
          jsx: true,
        },
      },
    },
    settings: {
      react: {
        version: "detect",
      },
    },
    rules: {
      "react/prop-types": "off",
    },
  },
  {
    files: ["*.js"],
    languageOptions: {
      globals: {
        ...globals.node,
      },
    },
  },
  {
    ignores: ["public/", ".cache/", "node_modules/"],
  },
]
