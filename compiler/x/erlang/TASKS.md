# Erlang Compiler Tasks

## Recent updates
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

## Remaining work
- [ ] Implement auto FFI handling for `go_auto` and `python_auto` programs.
- [ ] Fix nested accumulation bug in `group_items_iteration`.
- [ ] Support extern values for `python_math`.
- [x] Compile TPC-H query `q1.mochi` successfully.
- [x] Compile TPC-H queries `q2` through `q22` successfully.
