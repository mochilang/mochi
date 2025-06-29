# Mochi VS Code Extension

This extension adds language support for Mochi inside Visual Studio Code.  It bundles the TextMate grammar from `mochi-tm` and launches the `mochi-lsp` executable to provide diagnostics, completions and other editor features.

## Building

Install dependencies and package the extension:

```bash
npm install
npm run package
```

The resulting `mochi-0.1.0.vsix` can be installed via the Extensions view.  Ensure the `mochi-lsp` binary is on your `PATH` so the language server can start.
