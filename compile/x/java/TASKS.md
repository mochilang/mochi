# Java Backend TODOs for TPC-H Q1

The current Java compiler backend cannot successfully compile `tests/dataset/tpc-h/q1.mochi`.
Below is a list of tasks identified while attempting to add support:

1. **Implement numeric aggregate built-ins**
   - Add generic implementations of `sum`, `avg` and `count` that operate on
     `java.util.List` or Java arrays.
   - Update expression lowering to emit calls to these helpers.
2. **Fix dataset query code generation**
   - Generated Java for dataset queries ends with incomplete class definitions.
     Investigate `_query` helper emission for missing closing braces.
3. **Support group-by aggregates**
   - Ensure `_group_by` and related helpers work with aggregate functions inside
     the `select` clause.
4. **Add golden tests**
   - Once compilation succeeds, add `tpc-h_q1.mochi` to `tests/compiler/java` with
     expected Java output and runtime results.

