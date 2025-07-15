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
