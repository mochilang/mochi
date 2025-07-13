# Kotlin Compiler Tasks

## Recent Enhancements

- 2025-07-13 04:59 UTC: Added README checklist for TPC-H q1 and improved formatting.
- 2025-07-13 05:26 UTC: Added `div` helper for safe numeric division.
- 2025-07-13 15:41 UTC: Enabled compilation of TPC-H `q11` and updated tests.
- 2025-07-13 17:01 UTC: Added support for `q1` in Kotlin TPCH tests and improved
  selector handling for map-backed structs.

## Remaining Work
- [ ] Implement dataset join and group-by operations fully.
- [ ] Improve foreign import support (Python `math`, etc.).
- [ ] Compile the remaining unchecked programs.
- [ ] Get TPC-H `q2.mochi` compiling and running.
- [ ] Improve numeric division precision in queries.
