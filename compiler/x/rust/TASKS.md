# Rust Compiler Tasks

## Recent Enhancements
- 2025-07-18 01:30 - Added deterministic headers in `vm_golden_test.go` and
  switched to `golden.RunWithSummary` to report totals.
- 2025-07-18 00:10 - Replaced VM valid tests with `golden.Run` framework and
  removed code comparison from golden tests.
- 2025-07-17 16:30 - Improved map indexing to use `get()` and `insert` for
  `BTreeMap` values and regenerated VM golden tests.
- 2025-07-17 12:00 - Added golden tests for `tests/vm/valid` and fixed map indexing to reduce `.error` files.
- 2025-07-17 00:20 - Added `compile_rosetta_rust.go` and golden test for Rosetta tasks. Initialized mutable
  string variables with `String::new()` or `String::from` to avoid `+=` errors.
- 2025-07-13 05:07 - Added `_json` helper to print values using `Debug` trait and generate constant strings for simple map literals.
- 2025-07-13 05:08 - Generated Rust code for TPC-H queries q1 through q6 and extended golden tests.
- 2025-07-13 16:54 - Added golden output for `tpc-h` query q7 and updated tests to compile and run it.
- 2025-07-13 17:32 - Generated Rust code for TPC-H queries q10, q12, q13, q14, q15, q16 and q18 and extended golden tests.
- 2025-07-13 17:46 - Attempted to generate Rust code for q8, q9 and q11 but compilation failed due to numeric type mismatches.
- 2025-07-13 17:56 - Generated Rust code for TPC-H query q8 and added golden outputs.
- 2025-07-13 18:18 - Generated Rust code for TPC-H query q17 and added golden outputs.
- 2025-07-15 04:50 - Added golden tests for TPC-DS queries q1 through q19 and script to regenerate outputs.
- 2025-07-15 05:45 - Improved TPC-DS Rust updater to capture errors and write `.rs.out` files
- 2025-07-15 06:34 - Generated Rust code for additional TPC-DS queries and updated golden test to regenerate outputs automatically.
- 2025-07-15 07:09 - Improved field access for nested structs and regenerated TPC-DS Rust outputs as `.rs` files.
- 2025-07-15 07:24 - Mapped `any` type to `i32`, regenerated TPC-DS Rust outputs and removed obsolete `.rs.out` files.
- 2025-07-16 02:20 - Added Rosetta golden tests and implemented `f64` and `bool` casts to reduce `.error` files.
- 2025-07-18 02:00 - Fixed nested map indexing to avoid double dereference and
  cast unknown integer divisions to `f64` when the resulting field is a float.
- 2025-07-22 - Improved struct inference for query map literals so fields print
  without debug formatting. Updated VM golden outputs.
- 2025-07-24 - Inlined JSON strings for map variables and updated Rust VM README
  to reflect 100% compilation success.

- 2025-07-30 - Added _print_list helper and improved print builtin to remove trailing spaces. Generated Rust output for append_builtin, dataset_where_filter and values_builtin.
- 2025-07-17 - Improved type inference to inline numeric aggregates and print booleans as 1/0. VM tests pass 85/100.
- 2025-07-30 - Enhanced aggregate type inference and struct printing to reduce helper usage. VM tests pass 89/100.
- 2025-07-31 - Inlined list printing for primitive element types and updated VM outputs. VM tests pass 98/100.
- 2025-08-01 - Added compile-time YAML loading and JSONL saving to eliminate runtime helpers. VM tests pass 100/100.
- 2025-08-05 - Improved type inference for `append` and `json` builtins to inline
  operations when list and struct types are known. Updated VM golden outputs.
- 2025-08-06 - Inlined `append` calls for lists tracked from previous operations
  even when no explicit type is present.
- 2025-08-07 - Inlined JSON printing for struct literals by tracking constant
  values in `let` and `var` declarations.
- 2025-08-08 - Propagated constant JSON values through variable assignments so
  `json` can print them without runtime helpers.
- 2025-08-09 - Folded `append` calls with constant list arguments into a single
  vector expression to avoid cloning and pushing at runtime.
## Remaining Enhancements
- [ ] Validate generated code for `tpc-h/q1.mochi`
- [ ] Extend helper functions to support file I/O and dataset joins
- [ ] Format emitted Rust code more like `rustfmt`
