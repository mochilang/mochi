# Dart Compiler Tasks

## Recent Enhancements (2025-07-13 05:15 UTC)
- Added ability to compile `tests/dataset/tpc-h/q1.mochi` through `q22.mochi` and generate runnable Dart code.
- README in `tests/machine/x/dart` now tracks TPCH progress with nicer checklist.
- JOB queries `q1`–`q10` now compile and run. Golden code files have `.dart` suffix instead of `.dart.out`.
- [2025-07-13 16:53 UTC] Updated TPCH compiler script to generate and execute queries `q1`–`q15` successfully. Fixed map field access in generated code.
- [2025-07-15 04:44 UTC] Added `compile_tpcds_dart.go` and golden tests for TPC-DS queries `q1`–`q19`.
- [2025-07-15 04:57 UTC] Regenerated TPC-DS Dart outputs for all queries. Script now records `.error` files on failures.
- [2025-07-16 00:00 UTC] Escaped Dart reserved words and added `now()` builtin so more Rosetta tasks compile and run.
- [2025-07-16 11:59 UTC] Installed Dart SDK and regenerated Rosetta outputs. Added fallback `as` cast in compiler, dropping failing tasks from 193 to 189.
- [2025-07-16 15:21 UTC] Escaped newlines in string literals to prevent invalid Dart syntax and regenerated Rosetta outputs.

- [2025-07-16 15:30 UTC] Added golden tests for `tests/vm/valid` programs to verify Dart compilation output.
- [2025-07-18 00:00 UTC] Simplified VM golden tests to check runtime output only and removed outdated `ValidPrograms` test.
- [2025-07-16 17:28 UTC] Added `_print` helper and updated VM golden test to
  store outputs under `tests/machine/x/dart`, reducing `.error` files.
- [2025-07-18 12:00 UTC] Tweaked compiler to drop unnecessary `as int` casts,
  fixed integer division, and skipped trailing `return null` in match
  expressions. VM golden tests now print a pass/fail summary.
## Remaining Enhancements
- [ ] Improve generated code formatting to more closely match `tests/human/x/dart` examples.
