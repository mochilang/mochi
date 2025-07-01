# Ruby Backend Tasks

The Ruby backend now supports running the TPCH `q1` and `q2` examples, the first ten JOB queries and the TPCDS `q1` through `q99` queries. Generated code and runtime output for JOB `q1` through `q10` are checked in under `tests/dataset/job/compiler/rb`, TPCH output for `q1` and `q2` under `tests/dataset/tpc-h/compiler/rb` and TPCDS output for `q1` through `q99` under `tests/dataset/tpc-ds/compiler/rb`.

Implemented features:
- Grouping and query helpers via `_group_by` and `_query`.
- Struct values mapped to `OpenStruct` for convenient field access.
- Helper methods `sum`, `avg`, `count` and `json` for datasets.

Remaining work:
- Compile and verify the remaining JOB queries and TPCH queries beyond `q2`.
