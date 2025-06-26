# Prolog Backend Tasks for TPCH Q1

The Prolog backend emits facts and simple predicates. TPCH Q1 needs more advanced list processing.

- Represent each row as a fact and group rows using `findall/3` and dynamic predicates.
- Implement `sum_list/2`, `avg_list/2` and `count_list/2` for aggregations.
- Output JSON by constructing terms and using `json_write/2`.
- Add a golden test in `tests/compiler/pl` once supported.
