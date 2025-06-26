# COBOL Backend TODO

The current COBOL backend cannot compile the TPC-H Q1 example. The following work is required to support it:

1. **Group by support** – add code generation for `group by` clauses. Queries should accumulate items by key and expose a `Group` structure similar to the Go runtime.
2. **Aggregations** – implement built-in helpers for `sum`, `avg` and `count` that operate over lists and groups.
3. **Dynamic lists** – allow query results to grow at runtime rather than relying on fixed list lengths.
4. **Struct field access** – extend `expr()` to handle nested struct selectors used within queries.
5. **Golden tests** – once the above features compile Q1, add `tpc-h_q1.mochi`, `tpc-h_q1.cob.out` and `tpc-h_q1.out` to `tests/compiler/cobol`.
6. **Runtime helpers** – create small COBOL routines for JSON printing and numeric conversions if needed.

These tasks will bring the COBOL backend closer to running `tests/dataset/tpc-h/q1.mochi` and similar benchmarks.
