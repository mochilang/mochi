# Racket Backend Progress

Recent enhancements:

- 2025-08-30 05:04 - Added golden tests for `tests/vm/valid` to
  verify generated Racket code and runtime output.

- 2025-07-25 05:04 - Compiler now supports casts to and from `bigint`
  and honors `SOURCE_DATE_EPOCH` for reproducible outputs.

- 2025-07-21 05:04 - Added Rosetta Code golden tests and compile script
  `compile_rosetta_racket.go`.

- 2025-07-13 05:04 - Added string specialisation for `len` builtin. When the argument is a known string literal the compiler now emits `(string-length x)`.
- 2025-07-20 05:04 - TPCH queries `q1`-`q22` compile and run in golden tests.
- 2025-07-15 04:50 - TPCDS queries `q1`-`q99` now compile and run in golden tests.

## Remaining Work

- [ ] Keep generated code aligned with manual translations under `tests/human/x/racket`.
- [ ] Review query features such as `sort`, `skip`, and `take` for parity with other backends.

### 2025-09-10

- Added fallback YAML parser to remove dependency on external `yaml` package.
- Sort comparison now handles complex keys using `_lt`/`_gt` when values are not
  simple numbers or strings.

### 2025-10-05

- `avg` and `len` now emit simpler forms when operating on lists, matching the
  manual translations more closely.

- 2025-10-12 07:07 - `print` now omits runtime helpers for single string literals and outputs direct `displayln`.
- 2025-10-20 00:00 - Type inference now records builtin types so string
  variables print without runtime helpers.
### 2025-07-18

- Enhanced numeric and boolean inference to drop runtime helpers for
  comparisons and `print`. Added machine outputs for five more VM
  examples.
- 2025-07-18 - Added return type tracking for user-defined functions and
  boolean prints now output plain `#t`/`#f` without numeric conversion.
