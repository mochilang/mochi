# C++ Compiler Tasks

## Recent Enhancements (2025-07-13 09:34 UTC)
- Improved struct field type inference when the base variable's struct type is
  available only via a vector element type.
- Added constant numeric folding in binary expressions.
- Added unary constant folding for numeric and boolean values.
- Fallback to element type when variable struct info is missing in
  `structFromVars`.
- Added fallback to element type when inferring field types in map literals
  (2025-07-13 09:51 UTC).
- Updated map literal struct caching to allow later type refinement
  (2025-07-13 10:12 UTC).
- Updated `structFromVars` to refresh cached field types when more
  information becomes available (2025-07-13 10:44 UTC).
- Fixed struct field type replacement for grouped query key structs
  (2025-07-14 04:56 UTC).

## Recent Enhancements (2025-07-15 04:45 UTC)
- Generated missing TPC-DS outputs (q10-q49) with new `tpcds_golden_test.go`.
- Added deterministic timestamp via `SOURCE_DATE_EPOCH` for TPC-DS golden tests (2025-07-15 06:39 UTC).
- Enhanced TPC-DS golden tests to emit `.cpp` files and capture compilation errors (2025-07-15 06:57 UTC).

## Recent Enhancements (2025-07-15 07:14 UTC)
- Captured compilation failures for all TPC-DS queries via `tpcds_golden_test.go`.

## Recent Enhancements (2025-07-15 08:14 UTC)
- Extended TPCH golden tests to compile q1-q22 and capture build errors.
- Added `__any_eq` helper and improved type tracking for loop variables and
  list literals to reduce `std::any` usage (2025-07-16 11:41 UTC).
- Added golden tests for `tests/vm/valid` programs to ensure C++ output stays
  in sync and to automatically regenerate `.cpp` and `.out` files (2025-07-16
  15:24 UTC).

## Recent Enhancements (2025-07-16 16:35 UTC)
- Added fallback scalar type inference for struct fields referencing query
  variables when their struct information is missing.

## Recent Enhancements (2025-07-16 17:29 UTC)
- Simplified VM golden tests to check only runtime output and always emit
  generated `.cpp` and `.out` files.
- Improved `defineStruct` to replace field types referencing variables using
  either `varStruct` or `elemType` information.

## Recent Enhancements (2025-07-16 18:33 UTC)
- VM golden tests now log a summary of passed and failed programs using
  `golden.RunWithSummary`.
- Fallback to `std::any` in `structFromVars` when field types cannot be
  determined, reducing compile errors.

## Recent Enhancements (2025-07-17 00:52 UTC)
- `defineStruct` now falls back to `std::any` if unresolved variable
  references remain after replacement, avoiding g++ errors when struct
  field types reference undefined variables.
- Improved `compileStructLiteral` and `structFromVars` to replace
  leftover `decltype` placeholders with types inferred from `vars` or
  `elemType` (2025-07-17 01:30 UTC).

## Recent Enhancements (2025-07-17 02:00 UTC)
- `defineStruct` now treats unresolved `decltype(name)` field types as
  `std::any` when the referenced variable has no known type, reducing
  C++ compilation failures for grouped queries.

## Remaining Enhancements
- [ ] Improve formatting to better match human examples.
- [ ] Implement additional TPCH optimizations.

## Recent Enhancements (2025-07-17 12:07 UTC)
- Vector printing now matches Mochi VM output using brackets and commas.
- Map literal value type inference recognizes numeric literals to avoid
  `std::any` fallbacks.
- Double values are printed with one decimal place using `<iomanip>` helpers.
- Struct field types now update from `std::any` when later elements provide
  concrete types, reducing helper usage at runtime.

