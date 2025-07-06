# Clojure Converter

This package provides an experimental frontend that translates a limited subset of Clojure source code into Mochi.

## Architecture

`Convert` first attempts to parse the input by invoking `parse.clj` through the Clojure command line tool. The script emits a simplified JSON AST which is translated into Mochi by `programToMochi`. If the script is unavailable, the converter falls back to a running language server via LSP.

A very small parser written in Go is used for extracting function bodies and for interpreting the minimal AST nodes.

## Supported Features

- `defn` function definitions
- `def` variable bindings
- arithmetic expressions: `+`, `-`, `*`, `/`, `mod`, `quot`
- basic function calls and `println`
- returning a value via `throw (ex-info "return" {:value x})`

## Unsupported Features

Most of the Clojure language is not handled. Macros, complex data structures, namespaces, conditionals and loops are currently unsupported.
