# Lua Compiler Tasks

## Recent Enhancements (2025-07-13 09:40)
- Simplified `print` generation using direct string concatenation.
- Verified TPCH q1 output matches golden files.
- Added golden tests for JOB queries q21-q30.
 - TPCH q1-q22 now compile and run successfully. Golden tests validate
  runtime output only.
 - Introduced `tpch_golden_test.go` running q1-q2 and checking generated Lua
   code and output against golden files.

## Remaining Work
- [ ] Improve map pretty-printing for joins
