# Ruby Backend Tasks for TPCH Q1

The Ruby backend now supports running the TPCH Q1 example. Dataset queries
can combine filtering with grouping so more complex benchmarks compile
successfully.

Implemented features:
- Grouping and query helpers via `_group_by` and `_query`.
- Struct values mapped to `OpenStruct` for convenient field access.
- Helper methods `sum`, `avg`, `count` and `json` for datasets.
- Golden tests under `tests/dataset/tpc-h/compiler/rb` verify generated code
  and runtime output.
- Added `group_by_where.mochi` to `tests/compiler/rb` which exercises grouping
  with a `where` clause using the new helpers.
