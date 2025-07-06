# Scheme Converter

This package contains the experimental Scheme frontend used by the `any2mochi` tool. It converts a very small subset of Scheme source code into Mochi. When a Scheme language server is available it is used to extract symbols and hover information. If not, a lightweight `schemeast` helper parses the file.

The main entry point is `Convert` which returns Mochi code for a source string. `ConvertFile` reads a file and invokes `Convert`.

## Supported features

- Top level function and variable definitions
- Basic parameter extraction and return types when provided

## Unsupported features

- Full Scheme syntax and macros
- Advanced type inference and complex expressions
