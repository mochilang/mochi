# Ruby Compiler Tasks

## Recent Enhancements (2025-07-13 05:26)
- 2025-08-03 - Query aggregations now use Ruby list methods instead of helper
  functions when possible. Updated VM outputs.
- 2025-07-30 14:20 - Extended basic type inference in helper checks so built-in
  Ruby operations are used more often, reducing emitted runtime helpers. Added
  `bigint_ops` to machine outputs.
- Added versioned header comments to generated Ruby files.
- Recompiled all `tests/vm/valid` programs with the new header.
- Verified `tpc-h/q1.mochi` compiles and runs successfully.
- Verified `tpc-h/q2.mochi` and `tpc-h/q3.mochi` compile and run successfully.
- 2025-07-16 12:23 - Added nil checks in expression compilation to avoid
  panics during Rosetta runs and regenerated outputs.
- 2025-07-18 08:12 - Added `rosetta_golden_test.go` and `compile_rosetta_rb.go`
  for generating Ruby Rosetta outputs. Updated `print` handling for lists to
  better match expected output and regenerate files.
- 2025-07-16 15:30 - Added `vm_golden_test.go` for programs in `tests/vm/valid`
  to verify generated Ruby code and runtime output. Regenerated `.rb` and `.out`
  files with the `-update` flag.
- 2025-07-18 12:00 - Tweaked `_group_by` runtime helper to always unwrap joined
  rows which reduces `.error` files for dataset queries.
- 2025-07-20 09:15 - Removed obsolete `compiler_test.go` in favor of
  `vm_golden_test.go` and verified VM tests pass.
- 2025-07-21 13:45 - Added machine README with checklist of compiled programs.
- 2025-07-25 10:30 - Generated Ruby machine output for all `tests/vm/valid` programs.
- 2025-07-17 06:42 - Simplified `_sum` helper to use Ruby's native `sum`
  for clearer generated code. Regenerated VM and dataset outputs.
- 2025-07-17 07:01 - Compiled `bigint_ops.mochi` from `vm_extended/valid` and
  added results to machine checklist.

## Progress (2025-07-15 04:48)
- Recompiled `tpc-h` queries `q4`-`q22` using the updated compiler.
- Confirmed `tpch_golden_test.go` generates code and matches expected output.
- Verified all TPC-DS queries compile and run successfully via `tpcds_golden_test.go`.
- 2025-07-15 06:31 - Added script `compile_tpcds_rb.go` to regenerate Ruby TPC-DS outputs and updated all golden files.
- 2025-07-15 06:42 - Enhanced `tpcds_golden_test.go` to generate `.rb` files, capture `.error`, and compare outputs.

## Remaining Enhancements
- [ ] Format output closer to the examples in `tests/human/x/rb`.
- [ ] Extend dataset query support for additional TPC-H benchmarks.
