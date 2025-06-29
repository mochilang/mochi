# Mochi LSP

This directory provides a small Go package implementing the Language Server Protocol for the Mochi programming language.  It exposes helpers for diagnostics, document symbols, hover text, completions and definitions along with a reusable server implementation.

The command line entry point is `cmd/mochi-lsp`.  The binary communicates over stdio and can be used with any editor that speaks LSP.  The VS Code extension under `tools/vscode` automatically spawns this server when opened in the editor.

## Features

- Parse source files and run the static type checker to surface diagnostics with helpful error codes
- Provide document outline information via `documentSymbol` including the inferred type of each declaration
- Return hover text that shows the symbol's type and any preceding documentation comment
- Offer basic keyword completion items
- Jump to symbol definitions
- Search symbols across open files via `workspaceSymbol`
- Find references to a symbol via `references`
- Highlight all usages of a symbol in the current file via `documentHighlight`
- Rename a symbol across the workspace via `rename`

Much of this information comes from the `types` checker and built‑ins defined in `runtime/vm`.

Unit tests under this directory verify the analyser and server behaviour.

## Feature Checklist

Inspired by popular language servers such as [gopls](https://pkg.go.dev/golang.org/x/tools/gopls) and [rust-analyzer](https://rust-analyzer.github.io/), we track feature parity using the following checklist:

- [x] Diagnostics
- [x] Document symbols
- [x] Hover information
- [x] Keyword completions
- [x] Go to definition
- [x] Workspace symbol search
- [x] References
- [x] Document highlights
- [x] Rename
- [ ] Signature help
- [ ] Formatting support
- [ ] Semantic tokens
