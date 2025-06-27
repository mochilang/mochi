# Enhancing the C compiler for TPC-H Q1

The current C backend cannot compile dataset queries that use grouping. The implementation stops when encountering a `group by` clause and emits `return 0`.
Initial work added support for generating C structs and list helpers when a program contains a list of map literals. This allows datasets like `lineitem` in Q1 to be represented as native structs.

Relevant code showing the limitation:

```
// compileQueryExpr generates C code for simple dataset queries. It now
// supports optional `sort by`, `skip` and `take` clauses in addition to basic
// `from`/`where`/`select`. Joins and grouping remain unimplemented.
func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) string {
    ...
    // only handle simple queries without joins or grouping
    if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil {
        return "0"
    }
}
```
(see `compile/x/c/compiler.go` lines 913–965)

To run `tests/dataset/tpc-h/q1.mochi` the following work is required:

- [x] Implement map/struct generation for objects such as `lineitem` rows.
- [ ] Add runtime support for grouping rows by arbitrary keys (e.g. pair of strings).
- [ ] Extend `compileQueryExpr` to generate loops that build groups and compute aggregates.
- [ ] Emit helper functions for `sum`, `avg`, and `count` over lists of floats and ints.
- [ ] Add JSON serialization helpers for lists of structs/maps used by the query.
- [ ] Add golden tests under `tests/compiler/c` covering the new grouping logic.

Until these tasks are complete `mochi build --target c tests/dataset/tpc-h/q1.mochi` results in generated C that simply sets `result` to `0`.

## JOB dataset queries

Attempts to compile the JOB `q1.mochi`–`q10.mochi` programs fail because joins
and grouping are not supported yet. The backend stops early when `join` clauses
are present and emits `return 0`. Supporting these queries requires:

- Implementing join processing similar to the Go runtime.
- Extending grouping and aggregation support beyond simple lists.
- Emitting helper functions for `min` and other aggregations used in JOB.

