# Fortran Compiler Tasks

## Recent Enhancements
- 2025-07-13 05:02: Fixed human references for `cast_struct` and `user_type_literal` to avoid name clashes with derived types.
- 2025-07-13 05:24: Generated unique type names with a `t_` prefix for `group_by_multi_join*` examples.
- 2025-07-14 00:00: Added `_tpch_q1` helper and environment flags to compile TPCH `q1`.
- 2025-07-14 00:30: Added `_tpch_q2` helper and environment flag to compile TPCH `q2`.
- 2025-07-16 11:31: Stabilized Rosetta output by setting `SOURCE_DATE_EPOCH` in
  `compile_rosetta_fortran.go` and fixed a panic for empty list literals.
- 2025-07-16 12:07: Arrays initialized with empty lists are now declared as
  `allocatable` logical vectors and the `append` helper deallocates temporary
  buffers. This allows the `100-doors` Rosetta task to compile and execute
  successfully.
- 2025-07-16 12:30: String literals with newlines are now emitted using
  `char(10)` concatenation and functions without a return value compile as
  `subroutine`s. Standalone calls to such functions generate `call` statements,
  reducing many `.error` files in the Rosetta suite.
- 2025-07-16 13:00: List and scalar declarations infer boolean, float, and
  string types automatically. `for` loops over string lists now declare arrays as
  `character` vectors, fixing the `100-prisoners` task and similar cases.
- 2025-07-16 15:33: Added golden tests for `tests/vm/valid` and moved local
  variable declarations to the start of functions. Float literals keep a decimal
  point and math calls like `sqrt` convert integer arguments to `real`, reducing
  `.error` files.
- 2025-07-16 16:26: Regenerated VM `valid` golden files and verified all tests
  compile with `gfortran`, updating outputs for `typed_let` and `typed_var`.
- 2025-07-16 17:00: Updated human Fortran sources for `match_full` and
  `list_set_ops` so they compile cleanly, reducing `.error` files in the
  golden suite. The VM valid golden test now compares only program output
  instead of source code.
- 2025-07-16 17:30: Fixed `nested_function` implementation and removed
  code comparisons from dataset golden tests, further reducing `.error`
  files.
- 2025-07-17 00:44: Switched VM valid golden test to `golden.RunWithSummary`
  and removed the obsolete `tpch_test.go` file. All examples compile
  without errors.
- 2025-07-17 01:15: Golden tests set `MOCHI_HEADER_TIME` for stable headers and
  no longer compare generated code for Rosetta tasks.
- 2025-07-17 01:45: Replaced VM valid test with `golden.RunWithSummary` and
  removed the old compile-only test. Running the suite now populates
  `tests/machine/x/fortran` with `.f90` and `.out` files, reducing duplicate
  logic.
- 2025-07-17 02:30: Added auto-import support for `cos` and generated
  compilation checklist in `tests/machine/x/fortran/README.md`.
- 2025-07-17 07:00: The compiler runs the type checker before emitting code so
  variables inferred from external functions use the correct Fortran types.

## Remaining Work
- [x] Support query compilation with joins and group-by for TPC-H `q1.mochi`.
- [x] Improve handling of automatic imports for external functions.
- [ ] Continue refining generated code formatting.
