# Zig Converter

This package provides an experimental frontend for the `any2mochi` tool that converts a
subset of [Zig](https://ziglang.org/) source code into Mochi.

## Architecture

`Convert` first attempts to parse the source using the `zigast` helper
located under `tools/zigast`.  When this succeeds the extracted AST is
translated directly.  If parsing fails, the Zig language server is invoked and
symbol information is used to produce Mochi stubs.

The conversion focuses on function declarations and simple statements.
Types are mapped with `mapType` and statements are reconstructed by a small
parser implemented in Go.

## Supported Features

- Function definitions and parameters
- Basic control flow: `if`, `else if`, `else`, `while` and `for`
- Variable declarations and simple expressions
- Mapping of primitive Zig types to Mochi types

## Unsupported Features

- Generics and advanced type features
- Module imports and packages
- Pointer operations, unions and other complex Zig constructs
