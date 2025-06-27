# Kotlin Backend Tasks for TPCH Q1

The Kotlin backend now supports running the TPCH Q1 example.

Implemented features:
- Grouping and query helpers via `_group_by` and `_query`.
- Helper functions `_sum`, `_avg`, `_count` and `_json` for dataset processing.
- Golden tests under `tests/dataset/tpc-h/compiler/kt` verify generated code
    and runtime output.

## TODO

- Golden tests now exist for the JOB queries `q1` through `q10` but only `q1`
  compiles successfully.
- Code generation for the remaining JOB programs fails due to missing support
  for typed `Map` indexing and query argument casts. The resulting Kotlin source
  does not compile.
- Extend the compiler to infer proper map types when unpacking query arguments
  and update the generated code so that Kotlin's `[]` operator works.
- Once compilation succeeds, record the generated output under
  `tests/dataset/job/compiler/kt` and update the runtime expectations.
