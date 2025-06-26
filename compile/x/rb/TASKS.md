# Ruby Backend Tasks for TPCH Q1

The Ruby backend can compile control flow but not complex dataset operations.

- Implement grouping using `Enumerable#group_by` followed by `map` for aggregation.
- Map Mochi structs to Ruby `Struct` or classes with attribute accessors.
- Provide helper methods `sum`, `avg` and `count` over arrays.
- Serialize results with `JSON.generate` and add tests under `tests/compiler/rb`.
