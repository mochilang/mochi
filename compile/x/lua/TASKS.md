# Lua Backend Tasks for TPCH Q1

The Lua backend generates basic source but fails when `group by` is used.

- Implement grouping in the runtime using tables keyed by concatenated values.
- Provide helper functions `sum`, `avg` and `count` operating on Lua tables.
- Represent records as tables with string keys for TPCH data.
- Use a small JSON encoder (e.g., `cjson`) for output and add a golden test under `tests/compiler/lua`.
