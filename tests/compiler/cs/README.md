# C# to Mochi Conversion

This directory contains C# source snippets used to verify the `any2mochi` converter.
Each `*.cs.out` file is processed by the converter and compared against the
corresponding `.mochi` reference program.

## Supported Features

- Function declarations with parameters and return type detection via OmniSharp
- Struct and class field extraction
- Variable assignments and declarations for basic types
- `Console.WriteLine` mapped to `print`
- `if`/`else` statements
- `for` and `while` loops with numeric ranges
- Basic arithmetic and boolean expressions

## Unsupported Features

- Generics and LINQ query comprehension
- Exception handling and try/catch
- Async/await and tasks
- Attributes and preprocessor directives
- Complex pattern matching or switch expressions
