# F# to Mochi Converter

This directory stores golden files for converting F# source code to Mochi using the `fsautocomplete` language server.

## Supported Features
- Function signatures with parameter types inferred from hover information
- Basic statement parsing for generated F# code including:
  - `print` calls via `printfn`
  - `for` and `while` loops
  - simple variable assignments
  - return statements generated with `Return_*` exceptions

## Unsupported Features
- Complex pattern matching and discriminated unions
- Advanced type providers or generics
- Module and namespace declarations
- Preprocessor directives and attributes
