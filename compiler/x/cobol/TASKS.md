# COBOL Compiler Tasks

## Recent Enhancements
- 2025-07-23 00:00 - Unary operator formatting fixed so negative
  literals like `-3` are emitted correctly.
- 2025-07-20 00:00 - `print` now uses `WITH NO ADVANCING` to emit space
  separated values and a temporary string variable to avoid zero padded
  numbers.
- 2025-07-19 00:00 - Golden tests now use `golden.Run` and support `min`/`max` builtins for constant lists.
- 2025-07-18 16:00 - `vm_golden_test.go` now only verifies program output and
  invokes `cobc` with `-std=cobol2002` to compile more examples. Fewer `.error`
  files are generated in `tests/machine/x/cobol`.
- 2025-07-13 05:00 - Variable initialization expressions are compiled after the WORKING-STORAGE section so `if` expressions work.
- 2025-07-13 05:01 - Checklist in machine README updated; nested if outputs regenerated.
- 2025-07-16 15:30 - Added `>>SOURCE FORMAT FREE` directive and updated tests to
  invoke `cobc` with `-free`.
- 2025-07-18 09:30 - Added `vm_golden_test.go` for `tests/vm/valid` and fixed
  function result variable names.
- 2025-07-17 08:00 - Handle COBOL reserved words `NUMBER`/`NUMBERS` and replace
  `FUNCTION CONCATENATE` with a `STRING` statement. `break_continue` and
  `string_concat` now compile and run.
- 2025-07-21 00:00 - Added support for map and list element assignments and
  dynamic literal initialization. Temporary variables now use a signed PIC to
  preserve negative values.
- 2025-07-22 00:00 - Casts of string and int literals are now resolved at
  compile time and `len` of a string literal is folded to a constant. Nested
  function declarations are detected but code generation is still in progress.

## Remaining Work
- [ ] Support dataset queries (`group by`, joins) needed for TPCH programs such as `q1.mochi`.
- [ ] Generate helper routines for lists and maps to better match hand written examples.
- [ ] Add automated tests running the GNU COBOL compiler.
