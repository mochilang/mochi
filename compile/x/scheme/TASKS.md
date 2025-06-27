# Scheme Backend Tasks for TPCH Q1

The Scheme backend now targets chibi-scheme and is capable of running the
`tpc-h/q1.mochi` benchmark. Initial support for the JOB dataset was
experimented with by compiling `job/q1.mochi`. Compilation and execution for
`job/q1.mochi` through `job/q10.mochi` are covered by golden tests. Later
queries currently run but raise runtime errors because dataset operations are
not fully supported.

- Rows are represented as association lists and grouped using helper
  procedures.
- Aggregations `sum`, `avg` and `count` are implemented as simple folds.
- A minimal `json` builtin prints values for test output.
- Golden tests under `tests/compiler/scheme` verify the generated code and
  execution result for `tpch_q1.mochi`.
