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

## Remaining Work
- [x] Support query compilation with joins and group-by for TPC-H `q1.mochi`.
- [ ] Improve handling of automatic imports for external functions.
- [ ] Continue refining generated code formatting.
