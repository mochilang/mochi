# JVM Converter

This package provides a minimal converter from JVM assembly to Mochi code. It relies on a configured language server to parse the source. The main entry points are `Convert` for converting a source string and `ConvertFile` which reads a file and invokes `Convert`.

## Supported Features

- Extraction of function definitions via the language server

## Unsupported Features

- Detailed parsing of bytecode instructions
- Class or field extraction

