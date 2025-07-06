# Swift Converter

This package contains the experimental Swift frontend for the `any2mochi` tool.  Source files are parsed using `swiftc -dump-ast -dump-ast-format json` and a small portion of the resulting JSON is translated into Mochi code.

The entry points are `Convert` for converting a source string and `ConvertFile` which reads a file and calls `Convert`.

## Supported Features

- Function declarations and simple top level code
- Basic statements such as `if`, `else`, `for`, `return`
- Variable declarations with primitive types

## Unsupported Features

- Generics and complex type information
- Protocols, extensions and attributes
- Error handling constructs
