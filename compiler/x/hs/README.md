# Haskell Compiler

This package translates Mochi programs into Haskell source code. It was adapted from the archived implementation and supports a limited subset of the language.

Files:
- `compiler.go` – main compiler implementation
- `helpers.go` – helper utilities
- `runtime.go` – runtime support inserted into generated code
- `infer.go` – helper functions for type inference
- `tools.go` – ensures the Haskell toolchain is available and formats code
