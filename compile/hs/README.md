# Haskell Backend

The Haskell backend translates Mochi programs into regular Haskell source code. It is primarily intended for experimentation or running Mochi on systems that already have the GHC toolchain installed. Only a small subset of the language is implemented but it suffices for many scripts and utilities.

## Files

- `compiler.go` – main code generator walking the AST
- `compiler_test.go` – golden tests verifying generated code executes correctly
- `helpers.go` – helper utilities for indentation and naming
- `runtime.go` – minimal runtime helpers inserted on demand
- `tools.go` – ensures `runhaskell`/`ghc` are available for tests

## Building

Compile a Mochi program to Haskell using the build command:

```bash
mochi build --target hs main.mochi -o main.hs
```

The resulting `main.hs` can be executed with `runhaskell` or compiled with `ghc` just like any other Haskell program.

## Supported Features

The backend implements a small but practical subset of Mochi:

- Function definitions and variable bindings
- Basic expressions including `if`/`else` and arithmetic operators
- `for` loops over ranges or lists and `while` loops
- Lists and maps with literals and indexing
- Builtin helpers: `len`, `count`, `avg`, `str`, `print`, `input`, `now`, `json`, `load` and `save`

## Unsupported Features

Several language constructs remain unimplemented:

- `match` expressions and union types
- Dataset query syntax like `from ... sort by ...`
- Set literals and related operations
- Generative AI, HTTP fetch and FFI bindings
- Streams and long-lived agents
- Struct and object types
- Logic programming with `fact`, `rule` and `query` expressions
- Package imports and module system
- Concurrency primitives like `spawn` and channels
- Reflection or macro facilities
- Extern object declarations and package exports
- Functions with multiple return values
- Map membership operations
- `test` blocks and expectations
- Agent declarations, event emission and intent handlers
- Model declarations and extern variables/types

## Tests

Golden tests under `tests/compiler/hs` check both the produced Haskell code and its runtime behaviour. They are tagged `slow` because the Haskell toolchain is invoked:

```bash
go test ./compile/hs -tags slow
```
