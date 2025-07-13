# Erlang Compiler Tasks

## Recent updates
- [2025-07-13 05:02] Added support for `MOCHI_HEADER_TIME` and `MOCHI_HEADER_VERSION` environment variables in `meta.Header`.
- [2025-07-13 05:02] Inline constants when calling `contains` to avoid unbound variable errors.
- [2025-07-13 05:02] Regenerated machine outputs with consistent headers.

## Remaining work
- [ ] Implement auto FFI handling for `go_auto` and `python_auto` programs.
- [ ] Fix nested accumulation bug in `group_items_iteration`.
- [ ] Support extern values for `python_math`.
- [ ] Compile TPC-H query `q1.mochi` successfully.
