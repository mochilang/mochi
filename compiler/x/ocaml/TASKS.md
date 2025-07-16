# OCaml Compiler Tasks

## Recent Enhancements
- [2025-07-20 00:00] Added Rosetta golden tests and compile script.
- [2025-07-21 00:00] Added golden tests for `tests/vm/valid` programs and
  improved float list detection to reduce `.error` files.
- [2025-07-22 00:00] Fixed `update` statement generation and reworked VM tests
  to use outputs under `tests/machine/x/ocaml`.
- [2025-07-23 00:00] Handled casts inside `print` and sanitized OCaml field names
  to avoid reserved keyword errors.
- [2025-07-13 05:05] Improved group key type inference and numeric average handling.
- [2025-07-13 05:31] Added `sum_float` helper and logic to pick it when summing float lists.
- [2025-07-13 15:40] Generated code for TPCH queries q6–q10 and extended tests to run them.
- [2025-07-13 16:29] Generated code for TPCH queries q11–q15 and updated tests.
- [2025-07-13 16:53] Generated code for TPCH queries q16–q22 and updated tests.
- [2025-07-13 17:51] Regenerated TPCH OCaml code for q1–q22 with float sum fixes.
- [2025-07-15 04:49] Generated OCaml code for available TPC-DS queries and added tests.
- [2025-07-15 06:32] Added basic cast support for floats and list types and generated code for TPC-DS q51, q65, q90.

## Remaining Enhancements
- [ ] Verify floating point aggregates for full TPC-H q1
- [ ] Support more join variations inside grouped queries
