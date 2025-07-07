# OCaml Converter

This package contains the experimental OCaml frontend used by the `any2mochi` tool. It can convert a very small subset of OCaml code into Mochi.

The conversion prefers using the OCaml language server for symbol and type information. When `any2mochi.UseLSP` is disabled or the server is unavailable, a fallback parser implemented via `tree-sitter-ocaml` (see `ocaml_ast.js`) is used instead.

The main entry point is `Convert` which returns Mochi code for an input string. `ConvertFile` is a helper that reads a file and calls `Convert`.

## Supported features

- Function definitions
- Simple `let` bindings and `print_endline` statements (fallback parser)
- Basic primitive type mapping when using the language server

## Unsupported features

- Most of the OCaml language including modules and pattern matching
- Advanced type system features
