# Smalltalk Compiler Tasks

## Recent Enhancements
- 2025-07-19 - Golden tests now use `golden.Run` for programs in `tests/vm/valid` and store outputs under `tests/machine/x/st`. Removed the old duplicate test.
- Added fallback support for struct literals by emitting dictionaries. This reduces Rosetta `.error` files.
- Added golden tests for `tests/vm/valid` programs verifying generated
  Smalltalk code and runtime output.
- 2025-07-20 - Test suite now logs a pass/fail summary using `golden.RunWithSummary`.
  Removed obsolete `compiler_test.go` and stopped writing `.st.out` files.

## Remaining Work
- Support union type constructors.
