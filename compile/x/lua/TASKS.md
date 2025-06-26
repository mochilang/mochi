# Lua Backend Tasks for TPCH Q1

Support for TPCH Q1 is implemented in the Lua backend.

- Grouping is handled by `_group_by` which indexes tables by concatenated keys.
- Helper functions `sum`, `avg` and `count` operate on lists or groups.
- Records are emitted as tables with string keys for TPCH datasets.
- A minimal JSON encoder is bundled so tests run without external modules. The
  golden tests live under `tests/dataset/tpc-h/compiler/lua`.
