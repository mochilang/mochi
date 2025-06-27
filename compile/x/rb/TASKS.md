# Ruby Backend Tasks

The Ruby backend now supports running the TPCH `q1` and `q2` examples and the first ten JOB queries. Generated code and runtime output for JOB `q1` through `q10` are checked in under `tests/dataset/job/compiler/rb` and TPCH output for `q1` and `q2` under `tests/dataset/tpc-h/compiler/rb`.

Implemented features:
- Grouping and query helpers via `_group_by` and `_query`.
- Struct values mapped to `OpenStruct` for convenient field access.
- Helper methods `sum`, `avg`, `count` and `json` for datasets.

Remaining work:
- Compile and verify the remaining JOB queries and TPCH queries beyond `q2`.
