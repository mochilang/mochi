# Ruby Compiler Tasks

## Recent Enhancements (2025-07-13 05:26)
- Added versioned header comments to generated Ruby files.
- Recompiled all `tests/vm/valid` programs with the new header.
- Verified `tpc-h/q1.mochi` compiles and runs successfully.
- Verified `tpc-h/q2.mochi` and `tpc-h/q3.mochi` compile and run successfully.

## Progress (2025-07-13 16:48)
- Recompiled `tpc-h` queries `q4`-`q22` using the updated compiler.
- Confirmed `tpch_golden_test.go` generates code and matches expected output.

## Remaining Enhancements
- [ ] Format output closer to the examples in `tests/human/x/rb`.
- [ ] Extend dataset query support for additional TPC-H benchmarks.
