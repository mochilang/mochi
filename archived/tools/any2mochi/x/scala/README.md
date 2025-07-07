# Scala Converter

This package implements the experimental Scala frontend for `any2mochi`.
It supports a small subset of Scala and relies on a language server when
available.  When no server is configured a simple parser implemented in
`scalaast` is used as a fallback.

The primary entry points are `Convert` which converts a source string and
`ConvertFile` which converts a file.  The converter extracts top level
functions by inspecting document symbols or by parsing the fallback AST.

## Supported Features
- Function definitions
- Variable assignments using `val` and `var`
- Basic control flow (`if`, `for`, `while`, `else if`, `else`)
- Printing with `println`
- Simple return statements

## Unsupported Features
- Generics and advanced type annotations
- Pattern matching
- Complex class or object definitions
