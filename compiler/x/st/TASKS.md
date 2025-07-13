# Smalltalk Compiler Tasks

## Recent Enhancements
- 2025-07-13 05:03 - Added support for `else if` by recursively handling `IfStmt.ElseIf`.
- 2025-07-13 05:07 - Fixed header comments so generated files run under GNU Smalltalk.
- 2025-07-13 16:35 - Unary negation now parenthesizes expressions and grouping keys
  with complex expressions are wrapped. Initial work on compiling TPCH `q11`.
- 2025-07-13 17:00 - Added golden output for TPCH `q11` and updated tests to
  exercise queries 1-4.

## Remaining Work
- [ ] Verify TPC-H `q1.mochi` output matches reference when compiled.
- [ ] Improve generated code formatting for nested expressions.
