# Erlang Compiler Tasks

## Recent updates
- [2025-07-17 08:49] Used type inference to emit `maps:get` for known map fields,
  removing unnecessary `mochi_get` calls. Regenerated machine outputs.
- [2025-07-17 07:14] Changed Erlang comment prefix to `%%` for generated
  headers, matching the manual translations. Regenerated machine outputs.
- [2025-07-16 12:03] Improved mutation analysis to handle nested statements and
  replaced `rem()` calls with the `rem` operator. Regenerated sample Rosetta
  outputs.
- [2025-07-13 05:22] Removed support for `MOCHI_HEADER_TIME` and
  `MOCHI_HEADER_VERSION`. The compiler now always uses the repository
  version and the current UTC time.
- [2025-07-13 05:22] Regenerated Erlang machine outputs with dynamic headers.
- [2025-07-13 05:23] Added golden outputs for TPCH queries q16 through q21.
- [2025-07-13 16:26] Added golden output for TPCH query q22.
- [2025-07-13 17:40] Extended `tpch_golden_test` to run TPCH queries q1 through q22.
- [2025-07-13 05:02] Added support for `MOCHI_HEADER_TIME` and `MOCHI_HEADER_VERSION` environment variables in `meta.Header`.
- [2025-07-13 05:02] Inline constants when calling `contains` to avoid unbound variable errors.
- [2025-07-15 04:53] Generated Erlang code for TPC-DS queries q1-q99 using
  `compile_tpcds_erlang.go`. Runtime results are stored under
  `tests/dataset/tpc-ds/compiler/erlang` with `.error` files capturing
  failures.
- [2025-07-15 06:30] Regenerated TPC-DS Erlang outputs with `compile_tpcds_erlang.go`
  after updating the compiler to v0.10.26.
- [2025-07-15 07:16] Added smart map access fallback via `mochi_get` and
  regenerated failing TPC-DS outputs.
- [2025-07-15 07:25] Tweaked `mochi_get` to return `undefined` for missing
  keys and regenerated TPC-DS outputs, reducing `.error` count.
- [2025-07-17 00:15] Added Rosetta golden tests and `compile_rosetta_erlang.go`
  script to generate outputs. Fixed loop handling to reduce `.error` files.
- [2025-07-16 12:40] Improved `if` statement compilation to reuse variable names
  across branches, preventing unbound variable errors in several Rosetta
  examples. Regenerated outputs for the first few tasks.
- [2025-07-17 06:48] Implemented auto FFI support for `go_auto`, `python_auto`,
  and extern handling for `python_math`. Generated Erlang outputs for these
  programs.

## Remaining work
- [x] Implement auto FFI handling for `go_auto` and `python_auto` programs.
- [x] Fix nested accumulation bug in `group_items_iteration`.
- [x] Support extern values for `python_math`.
- [x] Compile TPC-H query `q1.mochi` successfully.
- [x] Compile TPC-H queries `q2` through `q22` successfully.
