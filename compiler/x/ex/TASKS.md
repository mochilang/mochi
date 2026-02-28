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

## Progress Update (2025-07-13 23:00)
- Re-ran TPCH golden tests; all 22 queries compile and execute successfully.
## Remaining Enhancements
- [x] Added `_now` helper so `now()` compiles and runs properly, reducing `.error` files in Rosetta tests.
- [x] Introduced `_length` helper and fallback string slicing logic to better handle dynamic `len()` and slice expressions. This fixes a few Rosetta tasks that previously crashed at runtime.
- [x] Updated `_input` to return an empty string on EOF so tasks expecting user
  input run without crashing.
- [ ] Finish YAML loader support for `load_yaml.mochi`.
- [x] Added basic YAML parsing in `_load` helper so `load_yaml.mochi` runs
  without errors.
- [x] `load` now maps rows to structs when a target type is provided, so
  `load_yaml.mochi` executes without runtime errors.
- [x] Fixed map membership compilation so `"a" in m` correctly checks maps with
  `Map.has_key?/2`, reducing `.error` files in VM tests.
- [x] Renamed TPC-DS generated sources from `.ex.out` to `.ex` and dropped code
  comparisons in golden tests.
- [ ] Improve formatting of generated code.

## VM Test Updates (2025-07-17 07:46)
- `print` now joins list elements with spaces so list output matches the VM runtime.
- Regenerated machine outputs under `tests/machine/x/ex`. 99/100 programs pass; `group_by_left_join.mochi` still fails at runtime.

## VM Test Updates (2025-07-17 07:59)
- Fixed left join grouping by introducing `_merge_map` helper and using it when merging joined rows.
- Regenerated machine output for `group_by_left_join.mochi`; now all 100 programs pass.

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

## VM Test Updates (2025-07-17 08:09)
- Regenerated machine outputs so all 100 programs under `tests/vm/valid` now have corresponding Elixir sources and output files.
- Loops now iterate over strings and groups without relying on the `_iter` helper.
- `len()` on groups is compiled to `length(g.items)` instead of calling `_length`.
- `concat` now emits `++` or `<>` when all arguments are lists or strings.
- `avg` uses integer division when the element type is integer so results match the VM.
- `min` and `max` call `Enum.min`/`Enum.max` for lists and groups.

## VM Test Updates (2025-07-17 12:10)
- `union` and related set operations inline list logic when operands are lists.
- `print` now uses `IO.inspect` for lists so outputs match the VM.
- Regenerated machine outputs for `append_builtin`, `values_builtin`, and `list_set_ops`.
- `len()` on lists now compiles to `length(list)` instead of calling `_length`.
  Regenerated machine outputs for `len_builtin`, `count_builtin`, and `list_set_ops`.
- String indexing and slicing now use `String.at/2` and `String.slice/3` when the
  operand is a string, so `_index_string` and `_slice_string` are rarely emitted.
- Function parameters are now typed within their bodies so built-ins like `len()`
  and `exists()` emit direct Elixir code. Regenerated machine outputs for
  `two-sum` and `exists_builtin`.

## Type Inference Enhancements (2025-07-18 08:13)
- `len` and `exists` now handle `option` values without relying on `_length`
  or `_exists`. When the underlying type is known the compiler emits direct
  Elixir code with a `nil` check.

## Rosetta Improvements (2025-07-18 15:56)
- Module attribute names are now lowercased. This fixes compilation errors for
  tasks defining constants like `SIZE` or `THRESHOLD`.
