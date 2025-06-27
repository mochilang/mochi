# Ruby Backend Tasks

The Ruby backend now supports running the TPCH `q1` example and the first ten JOB queries. Generated code and runtime output for JOB `q1` through `q10` are checked in under `tests/dataset/job/compiler/rb`.

Implemented features:
- Grouping and query helpers via `_group_by` and `_query`.
- Struct values mapped to `OpenStruct` for convenient field access.
- Helper methods `sum`, `avg`, `count` and `json` for datasets.

Remaining work:
- Compile and verify the rest of the JOB and TPCH queries.
