# Elixir Backend

The Elixir backend compiles Mochi programs to Elixir source code. It is used when targeting the BEAM runtime or for quick experimentation with Elixir tooling.

## Files

- `compiler.go` – walks the AST and emits Elixir source
- `expr.go` – expression helpers
- `runtime.go` – helper functions injected into generated code
- `tools.go` – ensures the `elixir` binary is available during tests
- `compiler_test.go` and `tpch_golden_test.go` – golden tests verifying generated programs run correctly

## TPC-H Progress

Queries `q1` and `q2` from the TPC-H suite compile and execute successfully. The generated Elixir code lives under `tests/dataset/tpc-h/compiler/ex`.
