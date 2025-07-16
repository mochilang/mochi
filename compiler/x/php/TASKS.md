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

## Remaining Work
- [ ] Improve runtime helpers for grouping and aggregation
- [ ] Keep machine output close to human reference implementations
