# Dart Backend Tasks for TPCH Q1

Dart compilation works for small programs but grouping in dataset queries is
incomplete.

- Lower `group by` expressions to loops building a `Map` of lists using Dart generics.
- Provide helper methods on `_Group` for `sum`, `avg` and `count`.
- Map Mochi structs to Dart classes so `lineitem` rows have typed fields.
- Serialize results with `dart:convert` and add a golden test under `tests/compiler/dart`.
