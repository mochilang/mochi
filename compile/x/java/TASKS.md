# Java Backend Tasks for TPCH Q1

The Java backend now runs `tests/dataset/tpc-h/q1.mochi` and the generated code
is checked in `tests/dataset/tpc-h/compiler/java`. Remaining tasks focus on
improving performance and coverage.

1. **Expand numeric helpers** – current implementations of `sum`, `avg` and
   `count` work for lists, arrays and groups. Support for additional numeric
   types could be added.
2. **Query optimisations** – dataset queries are functional but not optimised
   for performance. Pushing filters down and reducing allocations would help.
3. **Additional test coverage** – more complex TPC-H queries should be added
   as golden tests to ensure ongoing compatibility.
