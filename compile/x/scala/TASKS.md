# Scala Backend Tasks for TPCH Q1

Scala code generation now supports grouping and aggregation required for the
first TPC‑H query.

- Dataset queries are compiled to helper calls that perform filtering and
  grouping, emitting Scala collections.
- Aggregates such as `sum`, `avg` and `count` are handled when operating over a
  group.
- Results are serialised to JSON via a minimal runtime helper.
- `tests/compiler/scala/tpch_q1.mochi` exercises the new functionality.
