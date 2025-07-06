# Erlang Converter

This package provides an experimental Erlang frontend for the `any2mochi` tool. It relies on a small `escript` parser to extract function definitions from Erlang source files which are then translated into minimal Mochi code.

## Architecture

1. `parse.go` invokes `parser/parser.escript` to obtain a lightweight AST of functions.
2. `convert.go` walks this AST and emits Mochi `fun` definitions. Simple calls to `io:format` are rewritten as `print` statements.

`Convert` converts a source string, while `ConvertFile` is a helper that reads a file and calls `Convert`.

## Supported features

- Top-level function declarations
- Detection of `main/0` and insertion of a `main()` call
- Basic translation of `io:format/1` to `print`

## Unsupported features

- Module attributes and includes
- Pattern matching or guards
- Type specifications and records
- Anything beyond simple function bodies
