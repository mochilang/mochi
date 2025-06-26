# Java Backend TODOs

The Java backend now compiles `tests/dataset/tpc-h/q1.mochi`. Remaining items are
listed below for future improvements.

1. **Expand numeric helpers** – current implementations of `sum`, `avg` and
   `count` work for lists, arrays and groups. Support for additional numeric
   types could be added.
2. **Query optimisations** – dataset queries are functional but not optimised
   for performance. Pushing filters down and reducing allocations would help.
3. **Additional test coverage** – more complex TPC-H queries should be added
   as golden tests to ensure ongoing compatibility.

