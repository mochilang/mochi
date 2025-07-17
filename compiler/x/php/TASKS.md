# PHP Backend Tasks

## Recent Enhancements
- 2025-07-13 05:02 - Generated machine README includes dataset progress checklist.
- 2025-07-13 05:25 - Compiler handles TPC-H Q1 grouping output.
- 2025-07-13 15:53 - Compiler now supports TPC-H queries 6-8 with group key and
  match variable handling.
- 2025-07-13 15:53 - Added support for TPC-H queries 9-12 and updated golden
  tests.
- 2025-07-14 00:00 - Generated PHP code for TPC-H queries 13-22 and extended
  tests to cover all queries.
- 2025-07-13 17:42 - Fixed group variable detection so TPCH Q22 compiles with
  correct counts and regenerated golden outputs.
- 2025-07-15 06:32 - Added TPC-DS support. Generated PHP outputs for queries q1-q99 (except q51) via compile_tpcds_php.go and added tpcds_golden_test.go.
- 2025-07-16 00:10 - Added _print helper and FifteenPuzzleExample stub for Rosetta tests.
- 2025-07-16 12:13 - Generated PHP code for select Rosetta tasks via compile_rosetta_php.go.
- 2025-07-16 16:20 - Sanitizer now avoids PHP reserved names like `shuffle` and
  `this` so more Rosetta tasks compile successfully.
- 2025-07-17 00:00 - Added golden tests for VM valid programs to ensure PHP backend parity.

- 2025-07-17 12:00 - VM valid golden tests now use `tests/machine/x/php` for outputs and duplicate compiler test removed.
- 2025-07-17 12:30 - Golden tests verify runtime output only, dropping code comparisons.

- 2025-07-17 23:00 - Simple scalars now use `echo` instead of `_print` in generated PHP code.
- 2025-07-17 23:00 - Added README checklist for VM valid program outputs.

## Remaining Work
- [ ] Improve runtime helpers for grouping and aggregation
- [ ] Keep machine output close to human reference implementations
- 2025-07-17 01:10 - Marked VM golden tests with the "slow" build tag to avoid running during default test passes.

## July 2025 Progress
- 2025-07-19 13:20 - Verified that all 100 VM valid programs compile and run successfully with the PHP backend. Updated README checklist accordingly.
- 2025-07-20 10:00 - Generated PHP machine outputs for all VM valid programs and added README checklist.
- 2025-07-21 08:00 - Printing now uses `var_dump`; `_print` helper removed.
- 2025-07-21 12:00 - Avg builtin inlines numeric lists to avoid runtime helper.
