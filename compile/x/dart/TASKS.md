# Dart Backend Tasks for TPCH Q1

Dart compilation works for small programs but grouping in dataset queries is
incomplete. The following items outline the steps required to run
`tests/dataset/tpc-h/q1.mochi` with the Dart backend.

1. **Lower `group by` queries**
   - Build grouping loops that populate a `Map<String, _Group>` preserving the
     original row order.
   - Emit calls to the helper functions when aggregates appear in the `select`
     clause.
2. **Enhance `_Group`**
   - Provide instance methods `count`, `sum` and `avg` which delegate to the
     existing helper functions.
   - Keep the `key` value and `Items` list accessible for result construction.
3. **Typed struct generation**
   - Map Mochi `struct` definitions to Dart classes with typed fields and a
     `fromJson` constructor.
   - Use these classes when compiling dataset access so `lineitem` rows are
     strongly typed.
4. **Serialisation and testing**
   - Output results using `jsonEncode` from `dart:convert`.
   - Add a golden test under `tests/compiler/dart` that compiles and executes
     `q1.mochi` once the above pieces are in place.
