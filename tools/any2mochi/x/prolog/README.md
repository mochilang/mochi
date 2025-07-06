# Prolog Converter

This package implements a minimal Prolog frontend for the `any2mochi` tool. It converts simple Prolog predicates into Mochi functions.

## Architecture

- `convert.go` contains the high level conversion logic. It uses a language server when available and falls back to a small regex based parser.
- `parse_ast.go` invokes a small SWI-Prolog script (`pl_ast.pl`) to extract predicate information when the language server is not present.
- `pl_ast.pl` is a helper script run by `swipl` to emit JSON describing the parsed file.

## Supported Features

- Top-level predicate definitions mapped to `fun` declarations.
- Basic argument extraction from signatures.
- Handling of `write/1` and simple assignments using `is`.

## Unsupported Features

- Modules and multifile predicates.
- Complex control flow and most built-in predicates.
- Operator declarations or macro expansions.
