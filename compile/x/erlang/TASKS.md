# Erlang Backend Tasks for TPCH Q1

Dataset queries for TPCH Q1 are now fully supported by the Erlang backend.

- Grouping is handled by `mochi_group_by/2` which returns maps containing
  `key` and an `Items` list preserving order.
- Aggregation helpers `mochi_sum/1`, `mochi_avg/1` and `mochi_count/1` work on
  plain lists as well as groups.
- Results are printed as JSON via the built in helpers.
- Golden tests cover both `q1.mochi` and `tpch_q1.mochi` under
  `tests/compiler/erl`.
