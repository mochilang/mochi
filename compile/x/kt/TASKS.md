# Kotlin Backend Tasks for TPCH Q1

Dataset queries already map to Kotlin collection operators but grouping with aggregates is incomplete.

- Extend `_group_by` to handle complex keys and return lists of data classes.
- Use Kotlin extensions to compute `sum`, `avg` and `count` on grouped values.
- Ensure data classes for TPCH rows match the query structure.
- Output JSON via `kotlinx.serialization` and add tests in `tests/compiler/kt`.
