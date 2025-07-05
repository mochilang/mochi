# Go to Mochi Conversion

This directory contains Go source snippets and corresponding Mochi output used to test the `any2mochi` Go converter.

## Supported Features

- Functions with parameters and a single return value
- Variable declarations and assignments
- `if` and `for` statements (including range loops)
- Basic operations on slices, maps and structs
- Print statements translated to `print`

## Unsupported Features

- Generics and type switches
- Method declarations on types
- Multiple return values
- Complex `switch` statements and type assertions
- Concurrency primitives and channels
