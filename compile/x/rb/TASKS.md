# Ruby Backend Tasks

The Ruby backend previously targeted the TPCH `q1` example. It now compiles and
runs the JOB dataset queries `q1` through `q10`. Generated Ruby code and runtime
output are checked in under `tests/dataset/job/compiler/rb`.

Implemented features include:
- Grouping and query helpers via `_group_by` and `_query`.
- Struct values mapped to `OpenStruct` for convenient field access.
- Helper methods `sum`, `avg`, `count` and `json` for datasets.
- Golden tests under both `tests/dataset/tpc-h/compiler/rb` and
  `tests/dataset/job/compiler/rb` verify generated code and runtime output.
- Added `group_by_where.mochi` to `tests/compiler/rb` which exercises grouping
  with a `where` clause using the new helpers.
