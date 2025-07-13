# Fortran Compiler Tasks

## Recent Enhancements
- 2025-07-13 05:02: Fixed human references for `cast_struct` and `user_type_literal` to avoid name clashes with derived types.
- 2025-07-13 05:24: Generated unique type names with a `t_` prefix for `group_by_multi_join*` examples.

## Remaining Work
- [ ] Support query compilation with joins and group-by for TPC-H `q1.mochi`.
- [ ] Improve handling of automatic imports for external functions.
- [ ] Continue refining generated code formatting.
