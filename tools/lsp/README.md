# Mochi Language Server

This directory will contain a Go implementation of the Language Server Protocol (LSP) for Mochi. The server will provide editor integrations such as diagnostics, completion and go-to-definition.

## Parallel Tasks

The following tasks can be tackled in parallel to bootstrap the LSP server:

- [ ] **Create Go module** in `tools/lsp` and set up `main.go` entrypoint.
- [ ] **Initialize JSON-RPC connection** handling using `golang.org/x/tools` packages.
- [ ] **Implement text document sync** (open, change, close) and connect to the Mochi parser.
- [ ] **Surface compiler diagnostics** as LSP diagnostics on file save.
- [ ] **Add hover information** that displays type and definition snippets.
- [ ] **Provide go-to-definition** using symbol tables from the compiler.
- [ ] **Implement completion provider** with keywords and context-aware suggestions.
- [ ] **Support document formatting** by invoking `mochi fmt` on demand.
- [ ] **Expose workspace symbol search** to jump between modules.
- [ ] **Add unit tests** under `tools/lsp` to validate protocol handlers.

These features can evolve independently before being combined into a full language server.
