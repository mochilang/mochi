# Racket Converter

This package provides a minimal Racket frontend for the `any2mochi` tool. The
converter first attempts to use a configured Racket language server to obtain
symbols and type information. If no server is available it falls back to a tiny
parser implemented in Go.

## Supported Features

- Top level function, struct, class and enum definitions returned by the
  language server
- Simple variable definitions
- Basic loops produced by `(for ...)`
- Conversion of literals such as numbers, strings, lists and hashes

## Unsupported Features

- Macros and advanced Racket syntax
- Module imports and `require` forms
- Complex control flow or expressions outside the recognised subset
