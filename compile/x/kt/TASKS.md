# Kotlin Backend Tasks for TPCH Q1

The Kotlin backend now supports running the TPCH Q1 example.

Implemented features:
- Grouping and query helpers via `_group_by` and `_query`.
- Helper functions `_sum`, `_avg`, `_count` and `_json` for dataset processing.
- Golden tests under `tests/dataset/tpc-h/compiler/kt` verify generated code
    and runtime output.

## TODO

- The JOB benchmark queries `q1` through `q10` now have golden VM outputs.
- Kotlin code generation for these programs still fails to compile.
- Implement the missing features and add matching golden files under
  `tests/dataset/job/compiler/kt`.
