# Tasks to Enable TPCH Q1 on Racket backend

Compilation of `tests/dataset/tpc-h/q1.mochi` with the Racket backend fails with `group clause not supported`. The compiler currently only supports `group by` queries without additional clauses. To run the full query we need:

1. **Group with filters and aggregation**
   - Allow `group by` queries that also include `where`, `skip`, `take`, or `sort` clauses.
   - Emit `_group_by` calls followed by filtering and aggregation logic.
2. **Grouping with structured keys**
   - Support grouping by arbitrary expressions (e.g. records `{ returnflag: ... }`).
3. **Aggregate functions**
   - Ensure `sum`, `avg`, and `count` operate over groups in generated Racket code.
4. **Golden tests**
   - After extending the compiler, add `tpc-h_q1.mochi` to `tests/compiler/rkt` with matching `.out` and `.rkt.out` files.

Once these features are implemented, the Racket compiler should successfully build and execute TPC-H Q1.
