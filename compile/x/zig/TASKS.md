# Zig Backend Tasks for TPCH Q1

The Zig backend mirrors the C compiler but does not yet emit grouping code.

- Implement grouping using `std.AutoHashMap` with dynamic arrays for row lists.
- Generate `struct` definitions matching TPCH rows and provide aggregate helpers with iterators.
- Output JSON via `std.json` and add a test under `tests/compiler/zig`.
