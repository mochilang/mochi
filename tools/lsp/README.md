# Mochi LSP

This directory provides a small Go package implementing the Language Server Protocol for the Mochi programming language.  It exposes helpers for diagnostics, document symbols, hover text, completions and definitions along with a reusable server implementation.

The command line entry point is `cmd/mochi-lsp`.  The binary communicates over stdio and can be used with any editor that speaks LSP.  The VS Code extension under `tools/vscode` automatically spawns this server when opened in the editor.

## Features

- Parse and type check files to surface diagnostics
- Provide document outline information via `documentSymbol`
- Return hover text for identifiers
- Offer keyword completion items
- Jump to symbol definitions

Unit tests under this directory verify the analyser and server behaviour.
