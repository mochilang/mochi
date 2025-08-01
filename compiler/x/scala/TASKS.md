# Scala Compiler Tasks

## Recent Enhancements
- 2025-07-25 00:00 - Generators now include type annotations when element types
  are known, eliminating runtime map lookups in sort expressions.
- 2025-07-24 00:00 - Fixed `querySelectEnv` to propagate `Option` types for join
  variables. This removes extraneous helper case classes and allows `outer_join`
  and `right_join` to compile cleanly.
- 2025-07-23 12:00 - Improved list element inference for `append` and preserved
  `Option` types on selector expressions to remove unnecessary helpers
- 2025-07-22 00:00 - Improved detection of `Option` values in `if` expressions to
  reduce `.error` files for join examples
- 2025-07-13 05:13 - Top-level `let` bindings move inside `main` when no functions follow
- 2025-07-13 07:24 - Fixed `$` interpolation, improved `json` handling, GROUP BY keys use case classes, empty mutable lists default to `ArrayBuffer[Any]`
- 2025-07-13 16:29 - Added generated code and tests for TPCH queries q3 and q4
- 2025-07-13 17:00 - Generated code and tests for TPCH queries q5 and q6
- 2025-07-13 17:28 - Generated code and tests for TPCH queries q7 and q8
- 2025-07-15 05:01 - Added script to compile TPC-DS queries and golden tests
- 2025-07-15 05:02 - Attempted to compile all TPC-DS queries; current compiler fails on q1 and others
- 2025-07-15 05:44 - Archive script now captures `.error` files when `scalac` or runtime fails
- 2025-07-17 05:20 - `substring` now accepts `bigint` indices to avoid false type errors during Rosetta tests
- 2025-07-18 12:14 - Added `padStart` builtin and Scala code generation support to reduce Rosetta compile errors
- 2025-07-19 00:30 - Handle `padStart` as string method and keep string type during concatenation to reduce `.error` files
- 2025-07-20 00:10 - Added golden tests for `tests/vm/valid` and propagated list element types with `SetVarDeep`
- 2025-07-21 00:00 - Removed duplicate machine tests and typed empty buffers to reduce `.error` files
- 2025-07-21 05:00 - VM valid tests now use `RunWithSummary` and sanitize object names for files like `two-sum`
- 2025-07-22 12:00 - Improved map handling in `len` builtin and sort expressions; fixed map type inference for `var` bindings
- 2025-07-22 12:30 - Added zero-value defaults for typed `let` and `var` statements
- 2025-07-23 00:00 - List literals now emit case classes directly and detect struct maps in queries
- 2025-07-18 08:12 - Added context-aware typing for empty list and map literals
- 2025-07-16 00:00 - Added VM valid golden tests and cleaned up dataset tests


## Remaining Work
- [ ] Review generated Scala for idiomatic improvements
- [ ] Compile additional TPCH queries (q1–q8 done)
- [ ] Refine type inference for empty collections
