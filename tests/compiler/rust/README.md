# Rust to Mochi Conversion

This directory contains Rust source snippets used to verify the `any2mochi` Rust converter.

## Supported Features

- Functions with parameters and return types
- `struct` definitions and associated `impl` methods
- `enum` definitions and `match` expressions
- `for` and `while` loops with `break` and `continue`
- `if`/`else` blocks
- Basic macros like `println!`
- Constant and type alias declarations
- Trailing expressions at the end of functions
- Generic functions (type parameters are ignored)
- `Vec<T>` and slice types convert to Mochi lists
- Basic `Option<T>` unwrap to the inner type

## Unsupported Features

- Full generic type handling
- Trait definitions and implementations
- Module system and imports
- Complex macros beyond `println!`
- Pattern match guards and `if let` constructs
