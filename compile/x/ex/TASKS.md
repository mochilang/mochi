# Elixir Backend Tasks for TPCH Q1

The Elixir backend is experimental and lacks dataset features.

- Use `Enum.group_by` with filtering and ordering to implement query groups.
- Compile Mochi structs to Elixir structs for `lineitem` and related data.
- Provide aggregation helpers using `Enum.sum`, `Enum.count` and a custom average.
- Output JSON via `Jason.encode!` and add tests under `tests/compiler/ex`.
