# Java Backend Tasks for TPCH Q1

The Java backend compiles `tests/dataset/tpc-h/q1.mochi` and the generated code
is checked in `tests/dataset/tpc-h/compiler/java`. Initial golden files were
added for `dataset/job` queries `q1` and `q2`, however the emitted Java fails to
build because map field access is not handled correctly. Remaining tasks focus
on improving dataset support and performance.

## Pending JOB query support

Attempts to compile `dataset/job` queries `q1` through `q10` still produce
invalid Java. The generated source has mismatched braces and incorrect method
calls for map values. Further compiler work is needed before these queries can
be executed as part of the test suite.

1. **Expand numeric helpers** – current implementations of `sum`, `avg` and
   `count` work for lists, arrays and groups. Support for additional numeric
   types could be added.
2. **Query optimisations** – dataset queries are functional but not optimised
   for performance. Pushing filters down and reducing allocations would help.
3. **Additional test coverage** – more complex TPC-H queries should be added
   as golden tests to ensure ongoing compatibility.
