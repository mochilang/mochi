# TPC-H Query Support Status

The original Racket backend only handled the first TPC-H query. Support for
query 2 has now been added.

## Implemented Work

1. **Group with filters and aggregation** – `group by` queries may now include
   `where`, `skip`, `take` and `sort` clauses. The compiler emits a `_group_by`
   call and then performs the requested filtering and aggregation.
2. **Grouping with structured keys** – grouping by arbitrary expressions such as
   records is supported.
3. **Aggregate functions** – `sum`, `avg` and `count` operate over groups in the
   generated Racket code.
4. **Golden tests** – `tpc-h_q1.mochi` and `tpc-h_q2.mochi` with expected
   `.out` and `.rkt.out` outputs live in `tests/dataset/tpc-h/compiler/rkt`.

The Racket compiler can now build and execute TPC-H query 2. Query 1
compiles but the generated Racket source fails to run due to a syntax
error around the aggregation code.

Support for the first TPC-DS query has been prototyped. The generated
code lives under `tests/dataset/tpc-ds/compiler/rkt` alongside the
expected runtime output.
