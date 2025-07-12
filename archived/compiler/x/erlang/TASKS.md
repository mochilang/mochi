# Erlang Backend Tasks for TPCH Q1

The backend is now able to run the TPCH Q1 program and serialise
boolean values to JSON. Earlier revisions failed to compile
because the runtime helper `mochi_to_json/1` emitted invalid Erlang
syntax. With that issue resolved both Q1 and Q2 execute correctly
under `escript`.

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
Erlang code failed to run due to the same JSON helper issue. The
helper now emits valid syntax, allowing these programs to execute.
Golden files can therefore be generated once additional features are
implemented.

### TODO

- Once the remaining JOB programs execute reliably, add golden tests for
  `tests/dataset/job` covering `q1.mochi` to `q10.mochi` under
  `compile/x/erlang`.

## TPCH Q1–Q2 Status

Early attempts to generate Erlang code for `tests/dataset/tpc-h/q1.mochi`
and `q2.mochi` failed with `escript` reporting `syntax error before: true`.
This originated from invalid output in the `mochi_to_json/1` helper.  The
helper has since been corrected and both queries now compile and execute
successfully. The generated code and expected output have been added to
`tests/dataset/tpc-h/compiler/erlang` and are exercised by the golden
tests in `compiler/x/erlang`.
