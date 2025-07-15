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

## Remaining Work
- [ ] Implement dataset join and group-by operations fully.
- [ ] Improve foreign import support (Python `math`, etc.).
- [ ] Compile the remaining unchecked programs.
- [x] Get TPC-H `q2.mochi` compiling and running.
- [ ] Improve numeric division precision in queries.
