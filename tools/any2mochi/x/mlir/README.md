# MLIR Converter

This package contains an experimental MLIR frontend used by the `any2mochi` tool. It queries the MLIR language server and converts the discovered function symbols into minimal Mochi stubs.

## Supported Features
- Detects function symbols via language server
- Generates empty `fun` declarations for each function

## Unsupported Features
- Type information or function bodies
- Structs, globals and other declarations
