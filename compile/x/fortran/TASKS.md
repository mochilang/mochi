# Fortran Backend Tasks for TPCH Q1

The Fortran backend only handles simple array loops. Grouping and dynamic lists are missing. Additional work is required before `q1.mochi` can be compiled.

## TODO

- **Implemented:** generate code for the `sum()` builtin on numeric lists.
- Create derived types for TPCH rows (`LineItem`) and query results (`Q1Result`).
- Use `ALLOCATE` arrays and `stdlib` hash maps to build groups keyed by `returnflag` and `linestatus`.
- Provide helper functions `sum_int`, `sum_real`, `avg_int`, `avg_real` and `count_any` written in Fortran.
- Add a routine to print result arrays in a JSON-like format for the test harness.
- Once grouping works, add `q1.mochi` to `tests/compiler/fortran` with a golden file.
