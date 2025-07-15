# Lua Compiler Tasks

## Recent Enhancements (2025-07-13 09:40)
- Simplified `print` generation using direct string concatenation.
- Verified TPCH q1 output matches golden files.
- Added golden tests for JOB queries q21-q30.
- TPCH q1-q22 now compile and run successfully. Golden tests validate
  runtime output only.

## Remaining Work
- [ ] Improve map pretty-printing for joins

## Progress (2025-07-15 04:49)
- Generated Lua code for all TPC-DS queries using new helper script.
- Added golden test covering TPC-DS dataset to verify compilation and runtime output.

## Progress (2025-07-15 06:33)
- Extended TPC-DS golden tests to compare JSON outputs semantically.
- Verified queries q39, q40, q44, q47, q93, q97 and q99 compile and run without errors.

## Progress (2025-07-15 06:41)
- Regenerated Lua code and outputs for all TPC-DS queries using `compile_tpcds_lua.go`.
- Updated golden dataset to version v0.10.26; several queries still raise runtime errors.
