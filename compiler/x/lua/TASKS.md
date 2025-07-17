# Lua Compiler Tasks

## Recent Enhancements (2025-07-13 09:40)
- Simplified `print` generation using direct string concatenation.
- Verified TPCH q1 output matches golden files.
- Added golden tests for JOB queries q21-q30.
- TPCH q1-q22 now compile and run successfully. Golden tests validate
  runtime output only.

## Remaining Work
- [ ] Improve map pretty-printing for joins

## Progress (2025-07-15 04:49)
- Generated Lua code for all TPC-DS queries using new helper script.
- Added golden test covering TPC-DS dataset to verify compilation and runtime output.

## Progress (2025-07-15 06:33)
- Extended TPC-DS golden tests to compare JSON outputs semantically.
- Verified queries q39, q40, q44, q47, q93, q97 and q99 compile and run without errors.

## Progress (2025-07-15 06:41)
- Regenerated Lua code and outputs for all TPC-DS queries using `compile_tpcds_lua.go`.
- Updated golden dataset to version v0.10.26; several queries still raise runtime errors.

## Progress (2025-07-15 07:02)
- Added `compile_tpcds_lua.go` support for `.lua` output files.
- Regenerated TPC-DS Lua code and outputs after Lua installation.
- Extended `tpcds_dataset_golden_test.go` to run all queries using `.lua` goldens.

## Progress (2025-07-15 07:21)
- Fixed sorting comparator to handle complex keys without runtime errors.
- Regenerated Lua code and outputs for queries q21,q24,q41,q42,q44,q47,q49,q51,q52,q55.
- Removed old `.lua.out` files from dataset.

## Progress (2025-07-15 07:35)
- Updated query runtime to preserve nil join columns, fixing q40 and q93 runtime errors.

## Progress (2025-07-16 09:45)
- Added `compile_rosetta_lua.go` and golden tests for Rosetta examples.
- Implemented `__str` helper and improved `print`/`str` handling for lists and maps.
- Added support for `upper` builtin function.
- Generated Lua code and outputs for first few Rosetta tasks.

## Progress (2025-07-16 12:30)
- Updated Rosetta golden files using new `__print` helper.
- `4-rings-or-4-squares-puzzle` now passes and its `.error` file was removed.
- Added generated Lua code for additional tasks (e.g. `100-prisoners`).

## Progress (2025-07-17 08:10)
- Added golden tests for `tests/vm/valid` that compile programs with the Lua backend and verify runtime output.
- All examples compile and run successfully, producing 0 `.error` files.

## Progress (2025-07-17 09:00)
- VM valid tests now read golden outputs from `tests/machine/x/lua` and no longer use the `golden` helper.
- Removed old `TestLuaCompiler_ValidPrograms` duplicate.

## Progress (2025-07-17 10:15)
- Reworked `vm_golden_test.go` to regenerate Lua code and outputs using `golden.Run`.
- Running the golden tests confirmed all 100 programs compile and run successfully with no `.error` files.

## Progress (2025-07-18 08:45)
- Converted VM valid suite to use `golden.RunWithSummary`.
- Removed generated code comparisons from dataset tests and deleted the redundant JOB dataset suite.
- All Lua golden tests now focus on runtime output and store sources under `tests/machine/x/lua`.

## Progress (2025-07-18 12:00)
- Enhanced `__print` to support variadic arguments and trim trailing spaces.
- Typed variables now initialize to zero for basic numeric types.
- VM valid test pass count increased after addressing spacing and initialization issues.

## Progress (2025-07-18 12:45)
- Added basic `bigint` support via `tonumber` casts.
- Generated Lua code and output for `bigint_ops.mochi`.

## Progress (2025-07-18 13:15)
- Added bigint_ops from `vm_extended/valid` to Lua machine README; count now 101/101.
