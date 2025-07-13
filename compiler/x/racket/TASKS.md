# Racket Backend Progress

Recent enhancements:

- 2025-07-13 05:04 - Added string specialisation for `len` builtin. When the argument is a known string literal the compiler now emits `(string-length x)`.

## Remaining Work

- [ ] Investigate running `tests/dataset/tpc-h/q1.mochi` end-to-end.
- [ ] Keep generated code aligned with manual translations under `tests/human/x/racket`.
- [ ] Review query features such as `sort`, `skip`, and `take` for parity with other backends.
