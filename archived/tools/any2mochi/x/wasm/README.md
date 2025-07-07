# Wasm Converter

This package provides an experimental WebAssembly frontend used by the
`any2mochi` tool. It relies on an external language server to inspect a
module and generate minimal Mochi stubs for exported functions.

## Architecture

1. Source text is passed to the configured language server via LSP.
2. `DocumentSymbol` information is used to find exported functions.
3. Each function is emitted as a simple `fun` stub in Mochi.

## Supported Features

- Detection of top-level exported functions.
- Conversion of `.wasm` files through `ConvertFile`.

## Unsupported Features

- Local variables, memory and data segments.
- Import handling and complex module structures.
- Any behaviour beyond function stubs.
