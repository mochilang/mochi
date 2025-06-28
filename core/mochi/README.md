# `core/mochi`

This directory contains small self-hosted tools written in Mochi itself. They provide
simple reference implementations used by tests and help ensure the language can
boot‑strap its own tooling.

## Architecture

- **Parser** – `parser/parser.mochi` tokenizes source text and builds a minimal
  AST using a hand written recursive descent parser.
- **Interpreter** – `interpreter/interpreter.mochi` walks the AST and evaluates
  integer expressions and variable statements.
- **Type checker** – `types/check.mochi` performs basic static checks over the
  same toy AST.
- **Version utilities** – `version/version.mochi` parses and compares Mochi
  toolchain version strings.

These modules are intentionally tiny and support only a subset of Mochi.

## Supported features

- `let` and `var` declarations
- integer literals and identifiers
- unary negation
- binary operators `+`, `-`, `*`, `/`, `%`
- parentheses in expressions
- variable reassignment with `=`
- error reporting for unknown variables

## Unsupported features (partial list)

Many real language features are omitted:

- control flow like `if` or loops
- strings, floats and other literal types
- functions or closures
- user defined types and pattern matching
- package imports or I/O

These implementations are primarily for demonstration and test coverage.
