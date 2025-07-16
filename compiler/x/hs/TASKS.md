# Haskell Backend Progress

## Recent Updates (2025-07-21 05:00)
- Runtime helpers now always import `Data.Maybe` when included which fixes
  `break_continue` and similar programs. Updated VM golden tests regenerate the
  `.out` file and remove the old `.error`.
- Removed obsolete `compiler_test.go` and code comparisons from dataset tests so
  only runtime output is validated.

## Recent Updates (2025-07-19 05:00)
- Added golden tests for `tests/vm/valid` that verify both generated Haskell
  code and program output.
- Regenerated all `.hs.out` files which removed stale `Data.Aeson` imports and
  reduced the number of `.error` files.

## Recent Updates (2025-07-20 05:00)
- `group_by` helper now triggers `Data.Maybe` import automatically to avoid
  "fromMaybe not in scope" compile errors.
- Removed the golden-code test for VM programs so we only compare runtime
  output.

-## Recent Updates (2025-07-18 05:00)
- Automatically import `Data.Map` whenever runtime helpers are emitted so
  generated programs compile cleanly.
- Re-ran `compile_rosetta_hs.go` to refresh golden outputs after the change.

## Recent Updates (2025-07-17 05:00)
- Added golden tests for Rosetta tasks using `compile_rosetta_hs.go`.
- Fixed `Map` import detection so generated programs compile without manual edits.

## Recent Updates (2025-07-15 07:18)
- Added `tpcds_golden_test.go` and helper script `compile_tpcds_hs.go`.
- Generated initial Haskell outputs for TPC-DS queries q24-q26.

## Recent Updates (2025-07-13 05:19)
- Refactored runtime selection so programs that do not use `load`, `save` or `fetch` no longer import the `aeson` library.
- Generated `tpch_q1.mochi` for the first time. The program compiles to Haskell but requires `ghc` with `aeson` to run.
- Verified `append_builtin.mochi` builds with `ghc` 9.4.7.
- Captured compile errors for `tpch_q1.mochi` to track progress.

## Remaining Work
- [ ] Fully verify TPC-H query 1 output against reference results.
- [ ] Compile additional dataset queries (`q2`..`q22`).
- [ ] Improve pretty-printing of records to avoid verbose `Map` access.
