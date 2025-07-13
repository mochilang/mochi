# Scala Compiler Tasks

## Recent Enhancements
- 2025-07-13 05:13 - Top-level `let` bindings move inside `main` when no functions follow
- 2025-07-13 07:24 - Fixed `$` interpolation, improved `json` handling, GROUP BY keys use case classes, empty mutable lists default to `ArrayBuffer[Any]`

## Remaining Work
- [ ] Review generated Scala for idiomatic improvements
- [ ] Compile additional TPCH queries
- [ ] Refine type inference for empty collections
