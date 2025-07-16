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

## Progress (2025-07-15 07:45 UTC)
- Added `vm_golden_test.go` to validate programs under `tests/vm/valid`.
- Fixed function declarations with zero parameters so they no longer emit
  spurious commas in generated predicates.
- Golden tests now emit generated code as `.pl` files and record failures
  in `.pl.error` instead of skipping them.
