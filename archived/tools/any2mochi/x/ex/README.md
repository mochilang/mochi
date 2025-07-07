# Elixir Converter

This package contains the experimental Elixir frontend used by the `any2mochi` tool. It can convert a subset of Elixir source code into Mochi.

Two conversion paths are available:

1. `Convert` uses the Elixir language server to inspect source code.
2. `ConvertParsed` parses the source using a small regular-expression based parser.

`ConvertFile` and `ConvertFileParsed` are helpers that read files and invoke the respective functions.

## Supported

- Function definitions
- Basic control flow (`if`, `for`, `while`)
- Assignments and simple `IO.puts` calls

## Unsupported

- Macros and advanced pattern matching
- Modules, records and other complex structures
- Comprehensive type information
