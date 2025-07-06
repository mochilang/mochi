# Rust Converter

This package provides an experimental Rust frontend for the `any2mochi` tool. It relies on `rust-analyzer` to parse Rust source code and converts a limited subset of the language into Mochi.

## Architecture

1. `ParseAST` invokes `rust-analyzer` and converts its syntax tree into an `ASTNode` structure.
2. `Convert` runs `rust-analyzer` and emits Mochi code directly from the parsed tree.
3. `ConvertAST` performs the same conversion when an `ASTNode` is already available.

## Supported features

- Struct and enum declarations
- Constant and type alias definitions
- Top-level functions and `impl` methods
- Basic statements: `let`, `return`, `if`, `for`, `while`, `match`
- Primitive type mapping (`int`, `float`, `bool`, `string`, ...)

## Unsupported features

- Generics and trait bounds
- Most macros except `println!`
- Complex pattern matching or attributes
- Module system and visibility modifiers
