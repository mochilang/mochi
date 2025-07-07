# Ruby Converter

This package contains an experimental Ruby frontend used by the `any2mochi` tool. It converts a very small subset of Ruby source code into Mochi. Parsing relies on Ruby's built in `ripper` library to produce a simplified s-expression AST which is then translated to Mochi stubs.

The main entry points are `Convert` which returns Mochi code for a source string and `ConvertFile` which converts a source file.

## Supported Features
- `def` functions
- `class` declarations
- simple expressions and assignments
- `while` and `for` loops
- basic `if/else` statements
- `puts` calls mapped to `print`

## Unsupported Features
- modules and mixins
- blocks and lambdas
- metaprogramming constructs
- complex control flow and pattern matching
- detailed type information
