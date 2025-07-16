# COBOL Compiler Tasks

## Recent Enhancements
- 2025-07-18 16:00 - `vm_golden_test.go` now only verifies program output and
  invokes `cobc` with `-std=cobol2002` to compile more examples. Fewer `.error`
  files are generated in `tests/machine/x/cobol`.
- 2025-07-13 05:00 - Variable initialization expressions are compiled after the WORKING-STORAGE section so `if` expressions work.
- 2025-07-13 05:01 - Checklist in machine README updated; nested if outputs regenerated.
- 2025-07-16 15:30 - Added `>>SOURCE FORMAT FREE` directive and updated tests to
  invoke `cobc` with `-free`.
- 2025-07-18 09:30 - Added `vm_golden_test.go` for `tests/vm/valid` and fixed
  function result variable names.

## Remaining Work
- [ ] Support dataset queries (`group by`, joins) needed for TPCH programs such as `q1.mochi`.
- [ ] Generate helper routines for lists and maps to better match hand written examples.
- [ ] Add automated tests running the GNU COBOL compiler.
