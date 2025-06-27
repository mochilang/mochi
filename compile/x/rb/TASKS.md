# Ruby Backend Tasks

The Ruby backend supports running the TPCH Q1 example and now compiles the first
ten JOB queries. Dataset queries can combine filtering with grouping so more
complex benchmarks compile successfully.

Implemented features:
- Grouping and query helpers via `_group_by` and `_query`.
- Struct values mapped to `OpenStruct` for convenient field access.
- Helper methods `sum`, `avg`, `count` and `json` for datasets.
- Golden tests under `tests/dataset/tpc-h/compiler/rb` verify generated code
  and runtime output. Additional golden tests cover JOB queries `q1`–`q10` under
  `tests/dataset/job/compiler/rb`.
- Added `group_by_where.mochi` to `tests/compiler/rb` which exercises grouping
  with a `where` clause using the new helpers.
