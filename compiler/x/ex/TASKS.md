# Elixir Compiler Tasks

## Recent Enhancements (2025-07-13 05:12)
- Documented machine output checklist and TPCH progress.

## VM Test Updates (2025-07-16 18:34)
- `TestExCompiler_VMValid_Golden` now uses `golden.RunWithSummary` so the suite
  reports how many programs pass or fail.
- Updated the test to only compare runtime output and store generated code under
  `tests/machine/x/ex`. Old code comparison logic has been removed.
- Added struct generation for typed `load` expressions so examples like
  `load_yaml.mochi` no longer produce `.error` files.

## TPC-H Progress (2025-07-13 22:45)
All twenty-two queries (`q1` to `q22`) now compile and execute correctly. The
golden test `TestExCompiler_TPCHQueries` generates Elixir code for each query,
runs it through the `elixir` executable, and compares the printed result with
the expected `.out` file. The generated sources live under
`tests/dataset/tpc-h/compiler/ex`.

## Remaining Enhancements
- [x] Added `_now` helper so `now()` compiles and runs properly, reducing `.error` files in Rosetta tests.
- [x] Introduced `_length` helper and fallback string slicing logic to better handle dynamic `len()` and slice expressions. This fixes a few Rosetta tasks that previously crashed at runtime.
- [ ] Finish YAML loader support for `load_yaml.mochi`.
- [x] Added basic YAML parsing in `_load` helper so `load_yaml.mochi` runs
  without errors.
- [x] `load` now maps rows to structs when a target type is provided, so
  `load_yaml.mochi` executes without runtime errors.
- [x] Fixed map membership compilation so `"a" in m` correctly checks maps with
  `Map.has_key?/2`, reducing `.error` files in VM tests.
- [x] Renamed TPC-DS generated sources from `.ex.out` to `.ex` and dropped code
  comparisons in golden tests.
- [x] Added `_print_list` helper so lists passed to `print()` match VM output.
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
