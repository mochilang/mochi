# F# Converter

This package contains the experimental F# frontend used by the `any2mochi` tool.
It converts a very small subset of F# source code into Mochi.  The
implementation first attempts to query the `fsautocomplete` language server.
If that fails, a minimal regex based parser is used instead.

The main entry points are `Convert` and `ConvertFile`.

## Supported features

- `let` bindings converted to `let` statements
- `ignore (printfn "%A" (expr))` converted to `print(expr)`
- Basic function stubs discovered via language server

## Unsupported features

Everything else. Complex expressions, modules, generics and advanced
language features are ignored.
