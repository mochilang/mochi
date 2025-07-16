# Racket Backend Progress

Recent enhancements:

- 2025-07-21 05:04 - Added Rosetta Code golden tests and compile script
  `compile_rosetta_racket.go`.

- 2025-07-13 05:04 - Added string specialisation for `len` builtin. When the argument is a known string literal the compiler now emits `(string-length x)`.
- 2025-07-20 05:04 - TPCH queries `q1`-`q22` compile and run in golden tests.
- 2025-07-15 04:50 - TPCDS queries `q1`-`q99` now compile and run in golden tests.

## Remaining Work

- [ ] Keep generated code aligned with manual translations under `tests/human/x/racket`.
- [ ] Review query features such as `sort`, `skip`, and `take` for parity with other backends.
