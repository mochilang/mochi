# Dart Compiler Tasks

## Recent Enhancements (2025-07-13 05:15 UTC)
- [2025-07-17 01:58 UTC] Adjusted numeric type detection so map values cast to num, reducing VM failures.
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
- [2025-07-17 00:45 UTC] Normalized numeric printing so integers don't appear
  with trailing `.0` and set `SOURCE_DATE_EPOCH` in VM golden tests to generate
  stable headers.
- [2025-07-19 00:00 UTC] Improved builtin import handling and option checks in queries. Added fallback path search in `_load` helper.
- [2025-07-19 12:00 UTC] Fixed group join option handling, improved string comparisons, added repo helper, and all VM valid programs now pass.
- [2025-07-17 06:39 UTC] Print statements with a single argument now emit the
  built-in `print` function instead of `_print`, producing code closer to the
  hand-written examples. VM outputs regenerated.
- [2025-07-20 00:00 UTC] Added multiline formatting for list and map literals,
  improving readability of generated code.
- [2025-07-17 07:47 UTC] Improved equality handling so comparisons with `null`
  do not trigger the `_equal` helper, keeping generated programs smaller.
- [2025-07-20 12:00 UTC] Refined query type inference so result lists use
  specific element types instead of `dynamic`, reducing helper usage.
## Remaining Enhancements
- None.
