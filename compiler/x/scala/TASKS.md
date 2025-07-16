# Scala Compiler Tasks

## Recent Enhancements
- 2025-07-13 05:13 - Top-level `let` bindings move inside `main` when no functions follow
- 2025-07-13 07:24 - Fixed `$` interpolation, improved `json` handling, GROUP BY keys use case classes, empty mutable lists default to `ArrayBuffer[Any]`
- 2025-07-13 16:29 - Added generated code and tests for TPCH queries q3 and q4
- 2025-07-13 17:00 - Generated code and tests for TPCH queries q5 and q6
- 2025-07-13 17:28 - Generated code and tests for TPCH queries q7 and q8
- 2025-07-15 05:01 - Added script to compile TPC-DS queries and golden tests
- 2025-07-15 05:02 - Attempted to compile all TPC-DS queries; current compiler fails on q1 and others
- 2025-07-15 05:44 - Archive script now captures `.error` files when `scalac` or runtime fails
- 2025-07-17 05:20 - `substring` now accepts `bigint` indices to avoid false type errors during Rosetta tests

## Remaining Work
- [ ] Review generated Scala for idiomatic improvements
- [ ] Compile additional TPCH queries (q1–q8 done)
- [ ] Refine type inference for empty collections
