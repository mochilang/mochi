# ts2mochi

This package converts a limited subset of TypeScript into Mochi source code. It is
mainly used for testing the TypeScript backend by round tripping compiler output.

## Supported features

- Function declarations with numeric parameters
- `return` statements
- `console.log` as `print`
- Numeric literals and identifiers
- Array literals

## Unsupported features

The converter is intentionally small and does not understand most
TypeScript syntax such as loops, conditionals, classes, generics or
module systems.
