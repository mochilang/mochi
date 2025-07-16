# Pascal Compiler Tasks

## Recent Enhancements
- [2025-07-16 11:30] Fixed lambda compilation to preserve variable mappings and
  allowed `append` on untyped lists to reduce `.error` files for Rosetta tasks.
- [2025-07-16 12:10] Reordered global variable emission and helper placement so
  Pascal sources compile cleanly; deterministic headers for Rosetta tests.
- [2025-07-13 05:05] Installed FPC in CI environment and regenerated Pascal outputs.
- [2025-07-13 05:05] Fixed element type inference for grouped query results.
- [2025-07-28 15:50] Added Pascal compiler support for TPCH queries q11-q22 and updated dataset tests.
- [2025-07-15 06:32] Generated Pascal code and outputs for TPC-DS queries q50-q99 with new golden test and compile script.
- [2025-08-30 10:00] Added `values` builtin and golden tests for `tests/vm/valid` programs.
- [2025-09-01 09:15] Added VM golden tests and moved helper emission before
  variable declarations to reduce Pascal `.error` files.

## Remaining Work
- [x] Support advanced dataset queries required for TPC-H Q1.
- [x] Compile TPC-H queries q1-q5 end-to-end.
- [x] Compile TPC-H queries q6-q10 end-to-end.
- [ ] Ensure all programs in `tests/vm/valid` compile without manual edits.
