# Fortran Backend Tasks for TPCH Q1-Q2

The Fortran backend only handles simple array loops. Grouping, joins and dynamic lists are missing.

- Define derived types for TPCH rows and query results.
- Use `ALLOCATE` arrays and `stdlib` hash maps to build groups by key.
- Provide `sum`, `avg` and `count` helpers written in Fortran.
- Create a routine that prints JSON-like output for the test harness.
- Add `q1.mochi` and `q2.mochi` to `tests/compiler/fortran` once implemented.
- Implement join clauses for JOB queries and additional string helpers.
- Implement joins and grouping so that `q1.mochi` and `q2.mochi` compile successfully.
