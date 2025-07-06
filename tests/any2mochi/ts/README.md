# TypeScript to Mochi Conversion

This directory contains golden files used by `any2mochi` tests for converting TypeScript source code to Mochi.

## Supported features

- Function declarations with parameters and return types
- Basic variable declarations (`let`, `const`, `var`)
- Control flow in function bodies:
  - `return`
  - `if`/`else` blocks
  - `for ... of` loops
  - `while` loops
  - `break` and `continue`
  - `console.log` translated to `print`
- Type aliases and enums when reported by the language server
- AST parser now records start and end columns along with a snippet of the
  source for richer tooling and diagnostics

## Unsupported features

- Single line `if` statements without braces
- Complex expressions or generic type inference
- Switch statements and advanced class features
- Module imports and exports
