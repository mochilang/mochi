# OCaml Backend Tasks for TPCH Q1

The OCaml backend currently handles lists and simple loops only. Basic
aggregates and string helpers were added but full query support remains
incomplete.

- Compile `lineitem` and other records to OCaml record types with field access.
- Implement grouping using `Hashtbl` and compute aggregates with custom folds.
- Add helper functions for `sum`, `avg` and `count` over lists.
- Use the `yojson` library to serialize the final result.
- Include Q1 golden tests in `tests/compiler/ocaml`.
- Support `contains` on strings and lists.
- Provide `_min` helper and compile calls to `min()`.
- Extend `compileQueryExpr` to handle joins so JOB Q1 compiles.
