# Smalltalk Compiler Tasks

## Recent Enhancements
- 2025-07-16 12:20 - Fixed block syntax generation for functions without
  parameters. This allows Rosetta examples like `active-directory-connect`
  to compile without parse errors when executed under GNU Smalltalk.
- 2025-07-13 05:03 - Added support for `else if` by recursively handling `IfStmt.ElseIf`.
- 2025-07-13 05:07 - Fixed header comments so generated files run under GNU Smalltalk.
- 2025-07-13 16:35 - Unary negation now parenthesizes expressions and grouping keys
  with complex expressions are wrapped. Initial work on compiling TPCH `q11`.
- 2025-07-13 17:00 - Added golden output for TPCH `q11` and updated tests to
  exercise queries 1-4.
- 2025-07-15 06:30 - Added TPC-DS golden tests for Smalltalk compiler covering
  available queries.
- 2025-07-16 11:35 - Added Rosetta golden tests and implemented slice support for lists.
- 2025-07-17 08:50 - Python math imports are now compiled directly without runtime helper dictionaries.

## Remaining Work
- [ ] Verify TPC-H `q1.mochi` output matches reference when compiled.
- [ ] Improve generated code formatting for nested expressions.

