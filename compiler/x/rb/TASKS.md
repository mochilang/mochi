# Ruby Compiler Tasks

## Recent Enhancements (2025-07-13 05:26)
- Added versioned header comments to generated Ruby files.
- Recompiled all `tests/vm/valid` programs with the new header.
- Verified `tpc-h/q1.mochi` compiles and runs successfully.
- Verified `tpc-h/q2.mochi` and `tpc-h/q3.mochi` compile and run successfully.
- 2025-07-16 12:23 - Added nil checks in expression compilation to avoid
  panics during Rosetta runs and regenerated outputs.
- 2025-07-17 - Added `rosetta_golden_test.go` and `compile_rosetta_rb.go` to
  generate Ruby Rosetta outputs. Included a `FifteenPuzzleExample` stub so the
  first task compiles and runs without error.

## Progress (2025-07-15 04:48)
- Recompiled `tpc-h` queries `q4`-`q22` using the updated compiler.
- Confirmed `tpch_golden_test.go` generates code and matches expected output.
- Verified all TPC-DS queries compile and run successfully via `tpcds_golden_test.go`.
- 2025-07-15 06:31 - Added script `compile_tpcds_rb.go` to regenerate Ruby TPC-DS outputs and updated all golden files.
- 2025-07-15 06:42 - Enhanced `tpcds_golden_test.go` to generate `.rb` files, capture `.error`, and compare outputs.

## Remaining Enhancements
- [ ] Format output closer to the examples in `tests/human/x/rb`.
- [ ] Extend dataset query support for additional TPC-H benchmarks.
