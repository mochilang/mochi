# Python to Mochi Converter

This directory documents the Python converter used by `any2mochi`.
Conversion relies exclusively on the Python language server (pyright) to
discover symbols and type information.

## Supported Features

- Conversion of top level functions, classes and variables
- Function parameter and return types obtained via LSP hover requests
- Function bodies are included as comments for easier manual editing

## Unsupported Features

- Full statement and expression translation
- Decorators, generators and advanced Python features
