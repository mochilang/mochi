# Kotlin Compiler Tasks

## Recent Enhancements

- 2025-07-13 04:59 UTC: Added README checklist for TPC-H q1 and improved formatting.
- 2025-07-13 05:26 UTC: Added `div` helper for safe numeric division.
- 2025-07-13 15:41 UTC: Enabled compilation of TPC-H `q11` and updated tests.
- 2025-07-13 17:01 UTC: Added support for `q1` in Kotlin TPCH tests and improved
  selector handling for map-backed structs.
- 2025-07-13 17:27 UTC: Generated Kotlin code for `q12` and `q13`, enabled tests
  for `q2`, `q3`, `q12`, and `q13`.
- 2025-07-15 04:53 UTC: Added basic TPC-DS compile test and generation script;
  first three queries fail due to numeric comparison issues.
- 2025-07-15 05:08 UTC: Updated TPC-DS Kotlin generator to capture run errors and added golden tests comparing generated code and output.
- 2025-07-15 06:38 UTC: Generated Kotlin code for TPC-DS `q35`, `q43`, `q58`, `q59`, `q61`, and `q62`; updated tests to run these queries.
- 2025-07-15 07:18 UTC: Generated Kotlin code for additional TPC-DS queries (`q63`-`q69`, `q72`, `q78`, `q80`, `q82`-`q86`, `q89`, `q97`) and added golden test.
- 2025-07-15 07:48 UTC: Began implementing operator precedence handling in Kotlin compiler; initial attempt compiles but `q1` still fails due to type casting issues.
- 2025-07-16 11:40 UTC: Escaped reserved identifiers like `this`, fixed `now()` cast and skipped wrapper `main` when user-defined main exists.
- 2025-07-16 12:06 UTC: Empty list and map literals now use typed zero values when variables have explicit types, reducing Rosetta compile errors.
- 2025-07-16 12:42 UTC: Added `int` builtin support and deterministic headers, reducing Rosetta compile errors.
- 2025-07-16 14:00 UTC: Added VM golden tests for `tests/vm/valid` and removed
  stale `.error` files when compilation succeeds.
- 2025-07-16 15:00 UTC: Consolidated VM golden tests using `golden.Run` and
  removed code comparisons; outputs now written to `tests/machine/x/kotlin`.

## Remaining Work
- [ ] Implement dataset join and group-by operations fully.
- [ ] Improve foreign import support (Python `math`, etc.).
- [ ] Compile the remaining unchecked programs.
- [x] Get TPC-H `q2.mochi` compiling and running.
- [ ] Improve numeric division precision in queries.
