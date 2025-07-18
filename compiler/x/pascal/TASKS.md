# Pascal Compiler Tasks

## Recent Enhancements
- [2025-07-16 11:30] Fixed lambda compilation to preserve variable mappings and
  allowed `append` on untyped lists to reduce `.error` files for Rosetta tasks.
- [2025-07-16 12:10] Reordered global variable emission and helper placement so
  Pascal sources compile cleanly; deterministic headers for Rosetta tests.
- [2025-07-13 05:05] Installed FPC in CI environment and regenerated Pascal outputs.
- [2025-07-13 05:05] Fixed element type inference for grouped query results.
- [2025-07-28 15:50] Added Pascal compiler support for TPCH queries q11-q22 and updated dataset tests.
- [2025-07-15 06:32] Generated Pascal code and outputs for TPC-DS queries q50-q99 with new golden test and compile script.
- [2025-08-30 10:00] Added `values` builtin and golden tests for `tests/vm/valid` programs.
- [2025-09-01 09:15] Added VM golden tests and moved helper emission before
  variable declarations to reduce Pascal `.error` files.
- [2025-09-02 12:00] Simplified golden tests to only check runtime output and
  implemented `contains` method lowering for strings.
- [2025-09-03 15:45] Fixed detection of `contains` when used via selector
  to compile `string_contains` example successfully.
- [2025-09-04 10:20] Register temporary variables in the type environment and
  sanitize map literal generics to `Variant` when unknown, allowing
  `map_nested_assign` to compile.
- [2025-07-17 17:44] Added `max` builtin using `_maxList` helper and updated
  helper ordering; compiled `min_max_builtin` successfully.
- [2025-09-05 10:00] Enhanced group query inference and variable replacement
  so more VM valid examples compile (71/100 passing).
- [2025-09-06 09:40] Added field assignment support and procedure generation
  for functions without return values; compiled `record_assign` (72/100 passing).
- [2025-09-07 10:20] Improved loop variable inference for grouped queries and
  removed stale `.error` files; compiled `group_by` successfully (73/100 passing).

## Remaining Work
- [x] Support advanced dataset queries required for TPC-H Q1.
- [x] Compile TPC-H queries q1-q5 end-to-end.
- [x] Compile TPC-H queries q6-q10 end-to-end.
- [ ] Ensure all programs in `tests/vm/valid` compile without manual edits.
