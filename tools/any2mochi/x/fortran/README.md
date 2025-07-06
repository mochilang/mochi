# Fortran Converter

This package provides an experimental frontend for converting Fortran 90 source code into Mochi. It relies on the `fortls` language server to obtain symbol information and applies lightweight heuristics to translate simple statements.

## Supported features

- Detection of program and function declarations
- Basic parameter and return type inference
- Conversion of `print`, assignments, `if` blocks, `do` loops and `return` statements

## Unsupported features

- Complex numeric formatting and intrinsic functions
- Advanced control flow such as `select case`
- Modules with extensive `contains` sections
- Preprocessor directives and macros
