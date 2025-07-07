# OCaml Backend TODOs for TPCH Queries

The OCaml backend currently handles lists and simple loops only and cannot yet
compile TPCH queries that use grouping or joins.  Support for these features is
required before `q1.mochi` and `q2.mochi` can be compiled.

- Compile `lineitem` and related records to OCaml record types with field access.
- Implement grouping using `Hashtbl` and compute aggregates with custom folds.
- Implement join clauses for multiple sources.
- Add helper functions for `sum`, `avg` and `count` over lists.
- Use the `yojson` library to serialize the final result.
- Generate and check golden files for `q1` and `q2` under
  `tests/dataset/tpc-h/compiler/ocaml` once compilation succeeds.
