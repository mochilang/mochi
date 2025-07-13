# Elixir Compiler Tasks

## Recent Enhancements (2025-07-13 05:12)
- Documented machine output checklist and TPCH progress.

## TPC-H Progress (2025-07-13 22:45)
All twenty-two queries (`q1` to `q22`) now compile and execute correctly. The
golden test `TestExCompiler_TPCHQueries` generates Elixir code for each query,
runs it through the `elixir` executable, and compares the printed result with
the expected `.out` file. The generated sources live under
`tests/dataset/tpc-h/compiler/ex`.

## Progress Update (2025-07-13 23:00)
- Re-ran TPCH golden tests; all 22 queries compile and execute successfully.
## Remaining Enhancements
- [ ] Finish YAML loader support for `load_yaml.mochi`.
- [ ] Improve formatting of generated code.
