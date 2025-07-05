# Zig to Mochi Converter

This directory contains golden files for converting Zig source code to Mochi.

## Supported Features

- Top-level variable declarations
- Function definitions with parameter and return types
- Basic function body conversion including:
  - `return` statements
  - simple variable assignments
  - call expressions with casts removed

## Unsupported Features

- Complex control flow (loops, if/else)
- Struct and union definitions beyond field declarations
- Generic functions and advanced type features
- Error handling and defer blocks
