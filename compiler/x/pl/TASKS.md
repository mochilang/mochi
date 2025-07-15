# Prolog Compiler Tasks

## Recent Enhancements (2025-07-15 06:30 UTC)
- Added `compile_tpcds_pl.go` to generate Prolog code for TPC-DS queries.
- Introduced `tpcds_golden_test.go` which regenerates code via the script and verifies runtime output.
- Generated Prolog outputs for all available TPC-DS queries under `tests/dataset/tpc-ds/compiler/pl`.

## Progress (2025-07-15 07:21 UTC)
- Extended the Prolog backend with support for `null` literals and `strings.ToUpper`.
- Regenerated TPC-DS Prolog outputs for previously failing queries via `compile_tpcds_pl.go`.

