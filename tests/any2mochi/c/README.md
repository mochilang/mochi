# C to Mochi conversion

This directory stores golden files for converting C source to Mochi.

## Supported
- struct and type declarations
- function signatures
- simple function bodies (returns, loops, if/else, `while`, `do/while`)
- `printf` converted to `print`
- `scanf` converted to `input`
- fallback regex parser when the language server is unavailable

## Unsupported
- pointer operations and casts
- complex expressions and macros
- preprocessor directives
- advanced pointer arithmetic and memory management
