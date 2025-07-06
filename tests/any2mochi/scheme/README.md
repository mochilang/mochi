# Scheme to Mochi Converter

This directory stores golden files for converting Scheme source code back into Mochi using `tools/any2mochi`.

## Supported Features

- Detection of top level `define` forms via the language server or a simple fallback parser
- Function signatures with parameter names and return types when provided by the server
- Translation of common expressions including `display`, `set!`, arithmetic and indexing
- Basic control flow such as `if` statements and simple `while` loops emitted by the compiler
- Lambda expressions converted into `fun` literals when possible
- `cond` forms expanded into nested `if` statements
- Scheme identifiers are sanitized (e.g. `map-get` -> `map_get`)
- Quoted atoms like `'name` are turned into strings

## Unsupported Features

- Macros and complex control flow constructs
- Advanced syntax such as continuations or nested `let` blocks
- Detailed type information or module imports
