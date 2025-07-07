# Kotlin Backend Tasks for TPCH Queries

The Kotlin backend now supports running the TPCH `q1` example and the first nine
TPC‑DS queries. Query `q2` from TPCH compiles but cannot be executed in this
environment because the `kotlinc` tool is not available.

Implemented features:
- Grouping and query helpers via `_group_by` and `_query`.
- Helper functions `_sum`, `_avg`, `_count` and `_json` for dataset processing.
- Golden tests under `tests/dataset/tpc-h/compiler/kt` verify generated code and
  runtime output.
- Golden tests under `tests/dataset/tpc-ds/compiler/kt` now cover queries
  `q1` through `q19`.
- `q2` from TPCH needs golden runtime output recorded once `kotlinc` is
  available.

## TODO

- Install `kotlinc` to run TPCH Q2 and record the generated output under
  `tests/dataset/tpc-h/compiler/kt`.

- Golden tests now exist for the JOB queries `q1` through `q10` but only `q1`
  compiles successfully.
- Code generation for the remaining JOB programs fails due to missing support
  for typed `Map` indexing and query argument casts. The resulting Kotlin source
  does not compile.
- Extend the compiler to infer proper map types when unpacking query arguments
  and update the generated code so that Kotlin's `[]` operator works.
- Once compilation succeeds, record the generated output under
  `tests/dataset/job/compiler/kt` and update the runtime expectations.
