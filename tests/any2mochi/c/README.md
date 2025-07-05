# C to Mochi conversion

This directory stores golden files for converting C source to Mochi.

## Supported
- struct and type declarations
- function signatures
- simple function bodies (returns, loops including `while`, `for`, if statements, `printf` as `print`)
- increment/decrement and compound assignments

## Unsupported
- pointer operations and casts
- complex expressions and macros
- `switch` statements and advanced flow control
- preprocessor directives
