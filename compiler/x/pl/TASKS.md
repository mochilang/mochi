# Prolog Compiler Tasks

## Recent Enhancements (2025-07-15 06:30 UTC)
- Added `compile_tpcds_pl.go` to generate Prolog code for TPC-DS queries.
- Introduced `tpcds_golden_test.go` which regenerates code via the script and verifies runtime output.
- Generated Prolog outputs for all available TPC-DS queries under `tests/dataset/tpc-ds/compiler/pl`.

## Progress (2025-07-15 07:21 UTC)
- Extended the Prolog backend with support for `null` literals and `strings.ToUpper`.
- Regenerated TPC-DS Prolog outputs for previously failing queries via `compile_tpcds_pl.go`.

- Improved binary `+` handling so expressions use `string_concat` only when both operands are non-numeric. This prevents runtime failures on numeric additions in Rosetta tasks.
- Regenerated Rosetta outputs via `go test` with `UPDATE=1`.
- Installed `swipl` and regenerated golden outputs for a few Rosetta tasks
  (`100-doors-2` and `ackermann-function`) to verify the compiler end-to-end.

## Progress (2025-07-16 00:00 UTC)
- Implemented golden tests for programs under `tests/vm/valid`.
- Added closure support so captured variables are correctly passed to
  generated predicates.
  - Regenerated Prolog outputs via `go test -run TestPrologCompiler_GoldenOutput -update`.

## Progress (2025-07-17 01:33 UTC)
- Added `print_val/1` helper to format numeric output using `format('~g')`.
- Updated `isBoolExpr` to ignore operators inside quoted strings.
- Regenerated `python_math` outputs and marked the test as passing.

## Progress (2025-07-17 06:40 UTC)
- Fixed handling of zero-argument predicates so generated clauses omit the stray comma.
- Updated dynamic and static call generation to avoid an empty argument list.

## Progress (2025-07-27 00:00 UTC)
- Added short-circuit handling for `&&` and `||` in `buildBinary`.
- Extended `isBoolExpr` to recognize expressions containing `->` or `;`.
- Regenerated code for `bool_chain` using `go run ./cmd/mochix buildx -t pl`.

## Progress (2025-07-28 00:00 UTC)
- Added type-aware handling for indexing and slicing so lists and strings use native Prolog operations.
- Helper predicates `get_item` and `slice` are now omitted when not required.
