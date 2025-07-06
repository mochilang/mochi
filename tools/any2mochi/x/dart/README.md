# Dart Converter

This package provides an experimental Dart frontend for the `any2mochi` tool. It converts a subset of Dart source code into Mochi by invoking the `dartast` helper for parsing and using the Dart language server to enrich type information when available.

The main entry point is `Convert` which returns Mochi code for a given source string. `ConvertFile` is a small convenience wrapper for converting a file.

## Supported Features

- Top level functions
- Basic control flow (`if`, `for`, `while`, `else`)
- Variable declarations with `var`
- Simple expressions and `return` statements
- Enums, classes and method stubs via the language server

## Unsupported Features

- Full Dart syntax and semantics
- Generics beyond simple list/map detection
- Advanced language features such as mixins or async/await
