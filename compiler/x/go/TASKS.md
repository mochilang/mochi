# Go Compiler JOB Queries

The JOB dataset queries now build using the Go backend, but several
still fail their embedded tests. Generation results:

- `q1`, `q2` and `q6` compile and pass tests.
- `q3`, `q4`, `q5`, `q8`, `q9` and `q10` compile, but the generated
  programs report failed expectations.
- `q7` does not emit a Go file when built.

Golden outputs have been updated for the queries that compile. Further
work is required to make the failing programs pass their tests and to
determine why `q7` is skipped during compilation.

TPC-H progress:

- `q1` through `q22` compile and pass runtime checks using the Go backend.
  Golden code has been regenerated for all queries.

## Recent Updates
- 2025-07-14 07:19 - Removed `_print` helper usage; inline fmt.Println with string trimming for typed lists
- 2025-07-14 07:05 - Reverted fmt.Println optimisation for typed lists to match VM output
- 2025-07-14 06:18 - Added helper `isPlainAnyList` for future printing logic
- 2025-07-14 07:16 - Relaxed struct type comparison to match anonymous structs
- 2025-07-14 05:56 - Used `_toAnySlice` when converting to `[]any` and simplified print detection
- 2025-07-14 04:23 - Injected fallback struct generation to match human left join output
- 2025-07-14 03:18 - Removed inline closure for left join print; always emit inferred structs
- 2025-07-13 19:22 - Emitted struct declarations for query select results
- 2025-07-13 19:01 - Added explicit struct types for cross join outputs and dataset examples

- 2025-07-13 09:30 - Normalized `_convSlice` detection to avoid nested conversions

- 2025-07-13 07:13 - Ensured struct declarations are always written to the
  generated source.
- 2025-07-13 07:30 - Began verifying TPCH q1 runtime output
- 2025-07-13 08:04 - Skipped duplicate `_convSlice` calls and tidied printing
  logic
- 2025-07-13 09:17 - Avoided emitting new struct types when an identical
  definition already exists
- 2025-07-13 09:40 - Documented `compileMainFunc` behaviour
- 2025-07-13 10:46 - Simplified `formatDuration` for concise test output
- 2025-07-13 18:08 - Generated Go outputs for TPCH queries q1 through q22
- 2025-07-13 18:28 - Removed `_toAnyMap` helper; added `_copyToMap` and `_getField`
- 2025-07-13 18:41 - Generated `tpch/q1.go`; planning to drop helper functions
- 2025-07-14 01:45 - Added loop-based translation for simple left join; emits typed structs
- 2025-07-14 02:28 - Switched left join translation to use maps for fast lookup
- 2025-07-14 02:37 - Generated human-readable left join using map and singular struct names
- 2025-07-14 02:53 - Added conditional printf generation for simple left join print and capitalized ID field names
- 2025-07-14 04:38 - Avoided duplicate struct declarations by preserving seen set
- 2025-07-14 04:49 - Used fmt.Println for typed lists to match human output
- 2025-07-14 05:36 - Extended fmt.Println optimisation to struct slices
- 2025-07-14 06:41 - Regenerated TPCH golden Go files q1-q22 after fixing struct inference

## Remaining Work
* [ ] Recompile group_by variants with struct support
* [ ] Validate TPCH q1 runtime results
* [ ] Fix failing JOB query expectations
* [ ] Investigate missing output for JOB q7
* [ ] Audit generated code after `_toAnyMap` removal
* [ ] Remove helper functions prefixed with `_` from generated code
* [ ] Improve dataset query struct inference
* [ ] Improve singular name heuristics for inferred structs
* [ ] Compare generated TPCH q1 with human output
* [x] Extend fmt.Println optimisation to struct slices

- 2025-07-14 07:39 - Investigated TPCH q1 runtime build issues; added placeholder alias for v
- 2025-07-14 12:19 - Regenerated TPCH q1 Go code after fixing numeric casting

- 2025-07-14 12:49 - Attempted to generate TPCH Go files via tpch_golden_test; several queries still fail go run due to syntax errors.
- 2025-07-15 02:37 - Simplified group item generation and removed reflection from _avg/_sum helpers
- 2025-07-15 02:54 - Tweaked struct alias detection; prefer `Row` when available to fix TPCH generation
- 2025-07-15 03:06 - Extended JOB golden tests to run queries q1-q33 and added compile script for Go
- 2025-07-15 04:53 - Began implementing TPCDS support; added struct inference for group keys
- 2025-07-15 05:06 - Added TPCDS golden tests and refined typed loop handling

- 2025-07-15 05:53 - Captured compiler errors in TPCDS golden tests
- 2025-07-15 08:19 - Regenerated TPCH Go files for q14, q16, q17 and q19 after tweaking default alias logic
- 2025-07-15 08:32 - Regenerated TPCH Go code for q1-q22; all queries compile and run
- 2025-07-15 12:01 - Fixed nil map panic when compiling function literals; generated TPCDS Go files
- 2025-07-15 13:25 - Began adjusting group item generation for TPCDS q1; compile still fails

- 2025-07-16 00:58 - Regenerated TPCDS Go code; numerous queries still skip due to runtime mismatches
- 2025-07-16 01:32 - Added rosetta_golden_test.go and enabled `#` comments in the parser so more Rosetta tasks compile
- 2025-07-16 11:31 - Handled bool values in `_exists` and added type assertion for unary `!`; regenerated Rosetta Go outputs
- 2025-07-16 12:45 - Improved indexing on `any` by casting to `map[string]any` or `[]any`; infer simple map types from literals
- 2025-07-16 12:23 - Enabled full Rosetta golden tests for Go and expanded map
  inference using `stringKey`; mismatched map literals now unify key/value types
- 2025-07-16 13:10 - Added VM golden tests for valid programs and removed stale `.error` files after successful runs
- 2025-07-16 16:55 - Stabilised VM valid golden tests with fixed header time; new failing cases logged.
- 2025-07-16 23:59 - Replaced compiler_test.go with vm_golden_test.go using golden.Run and updated README generation
- 2025-07-19 08:01 - Updated golden helpers to delete `.mochi.error` files when running VM tests
- 2025-07-19 12:00 - Renamed VM test to vm_valid_golden_test.go and summarized pass/fail counts
- 2025-07-19 18:30 - Simple map literals now infer `map[string]T` before struct
  inference, allowing `for k in m` loops to compile. VM golden tests pass 78/100
  programs.

- 2025-07-20 00:00 - Updated print generation to trim trailing spaces; VM valid tests now pass 71/100 programs.
- 2025-07-20 12:34 - Added union type emission; tree_sum program compiles
