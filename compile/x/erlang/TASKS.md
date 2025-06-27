# Erlang Backend Tasks for TPCH Q1

The backend now supports running the TPCH Q1 program and serialising
boolean values to JSON.

- Rows are represented as maps and groups are built using `mochi_group_by`.
- Aggregation helpers `sum/1`, `avg/1` and `count/1` are implemented.
- Generated code includes JSON helpers for printing results including
  boolean literals.
- Golden tests under `compile/x/erlang/tpch_q1_test.go` verify code
  generation and execution of `tests/dataset/tpc-h/q1.mochi`.  Additional
  golden files in `tests/compiler/erl_simple` cover JSON output with
  booleans.

## JOB Dataset Status

Initial work attempted to compile JOB queries but the generated
Erlang code fails to run. `escript` reports a syntax error around the
`mochi_to_json(true)` helper when compiling `q1.mochi`. Queries
`q1` through `q10` therefore remain unsupported and golden tests have
not been generated yet.

### TODO

- Fix code generation of test blocks so anonymous functions compile
  correctly and captured variables are valid.
- Ensure `mochi_to_json/1` and other runtime helpers emit valid Erlang
  syntax.
- Once the programs execute, add golden tests for `tests/dataset/job`
  covering `q1.mochi` to `q10.mochi` under `compile/x/erlang`.
