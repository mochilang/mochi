# C++ Converter

This package provides an experimental C++ frontend for the `any2mochi` tool. It
uses a configured language server when available and falls back to parsing the
`clang++` JSON AST to produce minimal Mochi stubs.

## Supported Features

- Extraction of classes, structs, enums and functions via LSP document symbols
  when a C++ language server is available.
- Basic conversion of functions and enums using `clang++` when no server is
  present.
- Limited translation of simple function bodies (prints, returns and basic
  assignments).

## Unsupported Features

- Full C++ syntax or complex templates
- Advanced expression parsing
- Comprehensive type inference
