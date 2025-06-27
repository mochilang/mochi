# Haskell Backend Tasks for TPCH Q1

The Haskell backend supports basic lists, maps and loops. Grouping and
aggregations are still missing which limits support for the full TPCH and JOB
query suites.

- Introduce `data` declarations for TPCH rows and query results.
- Compile `group by` using `Data.Map` and `fromListWith` to accumulate groups.
- Provide folds implementing `sum`, `avg` and `count` on grouped values.
- Output JSON with `aeson` and add tests in `tests/compiler/hs` once working.
- Extend the runtime with heterogeneous map helpers so JOB queries `q2`–`q10`
  compile and run. Currently the generated code fails to type check because
  `VInt`/`VString` constructors are missing.
- TPCH queries `q1` and `q2` compile but the generated Haskell does not type
  check. `_writeOutput` and JSON helpers need to be incorporated into the
  runtime so these programs execute correctly.
