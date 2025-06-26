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
