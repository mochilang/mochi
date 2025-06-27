# Rust Backend Tasks for TPCH Q1

The Rust compiler now supports running the `tpc-h/q1.mochi` benchmark. Grouping
with optional filters is handled using `HashMap` and `Vec` and aggregate helpers
for `sum`, `avg` and `count`.

Completed tasks:

- [x] Use `HashMap` and `Vec` to implement grouping of rows by key.
- [x] Generate `struct` definitions for TPCH rows with `serde` `Serialize` implementations.
- [x] Provide iterator-based helpers for `sum`, `avg` and `count`.
- [x] Add a golden test in `tests/compiler/rust` once the query compiles successfully.
- [x] Support iterating over group values and updated builtins to use `_count`, `_avg` and `_sum` helpers.
- [x] Added TPCH q1 generated code under `tests/dataset/tpc-h/compiler/rust`.
- [x] Initial JOB dataset tests added for `q1` and `q2` under
  `tests/dataset/job/compiler/rust`.

## Remaining work

- [ ] Generate valid Rust for all JOB queries. Field access on map values
  currently emits invalid syntax like `m.field.contains` and functions like
  `min` are missing from the runtime.
- [ ] Extend code generation so that `q1` through `q10` compile and execute
  successfully. Runtime tests are skipped until these issues are resolved.
