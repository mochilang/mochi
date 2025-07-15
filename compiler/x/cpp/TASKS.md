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

## Remaining Enhancements
- [ ] Improve formatting to better match human examples.
- [ ] Support TPCH q1 and q5 compilation and execution.

