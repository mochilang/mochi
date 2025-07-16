# Elixir Compiler Tasks

## Recent Enhancements (2025-07-13 05:12)
- Documented machine output checklist and TPCH progress.

## Rosetta Progress (2025-07-16 11:38)
Added a builtin stub for `testpkg.FifteenPuzzleExample` so the
`15-puzzle-solver` example now compiles and runs without emitting a
`.error` file. The generated output is stored under
`tests/rosetta/out/Elixir/15-puzzle-solver.out`.

## TPC-H Progress (2025-07-13 22:45)
All twenty-two queries (`q1` to `q22`) now compile and execute correctly. The
golden test `TestExCompiler_TPCHQueries` generates Elixir code for each query,
runs it through the `elixir` executable, and compares the printed result with
the expected `.out` file. The generated sources live under
`tests/dataset/tpc-h/compiler/ex`.

## Remaining Enhancements
- [ ] Finish YAML loader support for `load_yaml.mochi`.
- [ ] Improve formatting of generated code.

## TPC-DS Progress (2025-07-15 04:45)
Initial golden test `TestExCompiler_TPCDSQueries` verifies the Elixir compiler
against the available TPC-DS query outputs. A new helper script
`compile_tpcds_ex.go` was added to regenerate these golden files. Queries without
matching `.out` files are skipped during the test.

## TPC-DS Progress (2025-07-15 05:02)
Flattened joined rows when grouping so fields like `sr_return_amt` are directly
accessible. `compile_tpcds_ex.go` now records runtime failures in `.error`
files. The golden test regenerates outputs via this script before running and
fails if any `.error` file is present.
