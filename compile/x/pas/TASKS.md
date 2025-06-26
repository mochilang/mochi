# Pascal Backend Tasks for TPCH Q1

Pascal code generation supports loops and arrays but not grouping or dynamic maps.

- Implement record types for dataset rows and allocate dynamic arrays for query results.
- Build groups using `TFPGMap` or similar container types from Free Pascal.
- Provide functions for `sum`, `avg` and `count` operating on arrays.
- Generate a small JSON printer and add tests under `tests/compiler/pas`.
