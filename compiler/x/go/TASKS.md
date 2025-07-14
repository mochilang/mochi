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

## Remaining Work
* [ ] Recompile group_by variants with struct support
* [ ] Validate TPCH q1 runtime results
* [ ] Fix failing JOB query expectations
* [ ] Investigate missing output for JOB q7
* [ ] Audit generated code after `_toAnyMap` removal
* [ ] Remove helper functions prefixed with `_` from generated code
* [ ] Improve dataset query struct inference
