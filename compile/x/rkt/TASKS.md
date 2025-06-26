# TPCH Q1 Support Status

The Racket backend now compiles `tests/dataset/tpc-h/q1.mochi` successfully.
Recent updates added:

1. **Group with filters and aggregation** – `group by` queries may include `where`, `skip`, `take` and sorting clauses. The compiler emits `_group_by` along with the required filtering logic.
2. **Structured grouping keys** – grouping expressions can be arbitrary values such as records.
3. **Aggregate functions** – `sum`, `avg` and `count` work over groups.
4. **Golden tests** – `q1.mochi` is included in `tests/compiler/rkt` with expected output and generated Racket code.

With these features implemented the backend can run the TPCH Q1 query.
