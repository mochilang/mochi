# TPC-H Q1 Support Status

The original Racket backend lacked support for grouping with additional clauses
which prevented `tests/dataset/tpc-h/q1.mochi` from compiling. The missing
features have now been implemented.

## Implemented Work

1. **Group with filters and aggregation** – `group by` queries may now include
   `where`, `skip`, `take` and `sort` clauses. The compiler emits a `_group_by`
   call and then performs the requested filtering and aggregation.
2. **Grouping with structured keys** – grouping by arbitrary expressions such as
   records is supported.
3. **Aggregate functions** – `sum`, `avg` and `count` operate over groups in the
   generated Racket code.
4. **Golden tests** – `tpc-h_q1.mochi` with expected `.out` and `.rkt.out`
   outputs lives in `tests/compiler/rkt`.

The Racket compiler can now build and execute TPC-H query 1.
