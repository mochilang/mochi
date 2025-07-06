# C Converter

This package provides an experimental C frontend for the `any2mochi` tool.
It converts a subset of C into Mochi. When a language server is available
(typically `clangd`), it is used to retrieve symbols and type information.
If no server is configured, the converter falls back to invoking `clang` to
produce a JSON AST.

The main entry point is `Convert` which accepts a source string. `ConvertFile`
reads a file and calls `Convert`.

## Supported Features

- Function declarations and simple bodies
- Structs, unions and enums
- Basic variable declarations

## Unsupported Features

- Preprocessor directives and macros
- Pointer arithmetic and complex casts
- Switch statements and other advanced control flow
