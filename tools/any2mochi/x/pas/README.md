# Pascal Converter

This package implements a minimal Pascal frontend for the `any2mochi` tools. It relies on an external Pascal language server when available but falls back to a very small built‑in parser.

## Architecture

* `Convert` parses a Pascal source string using a language server specified in `any2mochi.Servers`. If the server is not found the converter falls back to a simple regex based parser.
* `ConvertFile` is a helper that reads a file and calls `Convert`.
* The package walks the `DocumentSymbol` hierarchy returned by the language server and emits Mochi stubs.
* Basic statement conversion is handled in `convertBody` when no language server is present.

## Supported Features

* Function, method and type declarations.
* Simple variable declarations and assignments.
* Basic extraction of function bodies when a language server is unavailable.

## Unsupported Features

* Complete Pascal syntax coverage.
* Advanced control flow or expression translation.
* Comprehensive type analysis.
