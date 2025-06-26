# Haskell Backend Tasks for TPCH Q1

The Haskell backend supports lists and loops but no grouping of records yet.

- Introduce `data` declarations for TPCH rows and query results.
- Compile `group by` using `Data.Map` and `fromListWith` to accumulate groups.
- Provide folds implementing `sum`, `avg` and `count` on grouped values.
- Output JSON with `aeson` and add tests in `tests/compiler/hs` once working.
