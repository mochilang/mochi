# Ruby Backend Tasks for TPCH Q1

The Ruby backend now supports running the TPCH Q1 example.

Implemented features:
- Grouping and query helpers via `_group_by` and `_query`.
- Struct values mapped to `OpenStruct` for convenient field access.
- Helper methods `sum`, `avg`, `count` and `json` for datasets.
- Golden tests under `tests/dataset/tpc-h/compiler/rb` verify generated code
  and runtime output.
