# Smalltalk Converter

This package contains an experimental Smalltalk frontend for the `any2mochi` tool. It converts a minimal subset of Smalltalk source code to Mochi. The conversion first tries to use the Squeak language server. When the server is unavailable the converter falls back to a very small regex based parser defined in `parse_cli.go`.

The main entry points are `Convert` and `ConvertFile` from `convert.go` which return Mochi code for a Smalltalk source string or file.

## Supported Features

- Global variable assignments
- `print` statements
- `return` expressions
- Simple `while` loops
- Basic class declarations with fields and methods

## Unsupported Features

Any syntax beyond the constructs listed above is ignored or commented in the generated Mochi code. Complex method bodies, nested classes, imports, packages and most Smalltalk expressions are not supported.
