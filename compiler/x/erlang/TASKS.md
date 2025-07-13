# Erlang Compiler Tasks

## Recent updates
- [2025-07-13 05:22] Removed support for `MOCHI_HEADER_TIME` and
  `MOCHI_HEADER_VERSION`. The compiler now always uses the repository
  version and the current UTC time.
- [2025-07-13 05:22] Regenerated Erlang machine outputs with dynamic headers.
- [2025-07-13 05:23] Added golden outputs for TPCH queries q16 through q21.
- [2025-07-13 16:26] Added golden output for TPCH query q22.
- [2025-07-13 16:48] Enabled golden tests for TPCH queries q1 and q16-q22.
- [2025-07-13 05:02] Added support for `MOCHI_HEADER_TIME` and `MOCHI_HEADER_VERSION` environment variables in `meta.Header`.
- [2025-07-13 05:02] Inline constants when calling `contains` to avoid unbound variable errors.

## Remaining work
- [ ] Implement auto FFI handling for `go_auto` and `python_auto` programs.
- [ ] Fix nested accumulation bug in `group_items_iteration`.
- [ ] Support extern values for `python_math`.
- [x] Compile TPC-H query `q1.mochi` successfully.
- [x] Compile TPC-H query `q22.mochi` successfully.
