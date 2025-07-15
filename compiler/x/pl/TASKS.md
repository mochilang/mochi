# Prolog Compiler Tasks

## Recent Enhancements (2025-07-15 06:30 UTC)
- Added `compile_tpcds_pl.go` to generate Prolog code for TPC-DS queries.
- Introduced `tpcds_golden_test.go` which regenerates code via the script and verifies runtime output.
- Generated Prolog outputs for all available TPC-DS queries under `tests/dataset/tpc-ds/compiler/pl`.

## Recent Enhancements (2025-07-15 07:05 UTC)
- Regenerated all TPC-DS Prolog sources with reproducible headers using
  `SOURCE_DATE_EPOCH=0` to ensure deterministic golden tests.

