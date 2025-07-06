# Haskell Converter

This package provides an experimental Haskell frontend for the `any2mochi` tool. It converts a very small subset of Haskell into Mochi code. Parsing is performed by a tiny built‑in parser and the Haskell language server when available.

## Architecture

`Convert` invokes the language server to extract symbols and falls back to the built-in parser for trivial programs. `Parse` returns a lightweight AST used for this fallback.

## Supported Features

- Top level functions, types and variables detected by the language server
- Basic function signatures and field types
- Simple `main` functions using `putStrLn`

## Unsupported Features

- Pattern matching and advanced syntax
- Modules, instances and type classes beyond simple stubs
- General expression parsing outside of the fallback logic
