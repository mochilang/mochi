# Java Converter

This package provides an experimental Java frontend for the `any2mochi` tool. It relies on the `mochi-javaast` helper to parse Java source and translates a very small subset of the language to Mochi.

## Supported features

- Struct-like class definitions
- Simple static functions
- Variable assignments and basic expressions
- `for` and `while` loops
- Basic dataset helper functions

## Unsupported features

- Generics and advanced type features
- Inheritance or interfaces
- Exception handling
- Most standard library APIs
