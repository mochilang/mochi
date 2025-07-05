# Fortran to Mochi Converter

This directory contains golden files for converting Fortran source code to Mochi using the `fortls` language server.

## Supported
- Detection of program and function declarations
- Basic parameter and return type inference
- Simple statement conversion inside functions including:
  - `print` statements
  - variable assignments
  - `if` blocks and `do` loops
  - `return` statements

## Unsupported
- Complex numeric formatting and intrinsic functions
- Advanced control flow and `select case`
- Modules with contains sections beyond top-level functions
- Preprocessor directives and macros
