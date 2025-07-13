# Rust Compiler Tasks

## Recent Enhancements
- 2025-07-13 05:07 - Added `_json` helper to print values using `Debug` trait and generate constant strings for simple map literals.
- 2025-07-13 05:08 - Generated Rust code for TPC-H queries q1 through q6 and extended golden tests.
- 2025-07-13 16:54 - Added golden output for `tpc-h` query q7 and updated tests to compile and run it.
- 2025-07-13 17:32 - Generated Rust code for TPC-H queries q10, q12, q13, q14, q15, q16 and q18 and extended golden tests.
- 2025-07-13 17:46 - Attempted to generate Rust code for q8, q9 and q11 but compilation failed due to numeric type mismatches.
- 2025-07-13 17:56 - Generated Rust code for TPC-H query q8 and added golden outputs.
- 2025-07-13 18:18 - Generated Rust code for TPC-H query q17 and added golden outputs.

## Remaining Enhancements
- [ ] Inline JSON printing for variables when values are known at compile time
- [ ] Validate generated code for `tpc-h/q1.mochi`
- [ ] Extend helper functions to support file I/O and dataset joins
- [ ] Format emitted Rust code more like `rustfmt`
