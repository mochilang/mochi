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

- Generated C for JOB queries currently fails to compile, with undefined identifiers
  and invalid struct initialization. Once join and grouping logic are implemented,
  ensure the output builds successfully and compare against the existing `.out`
  results.

- Add golden tests under `compile/x/c/job_golden_test.go` covering `q1.mochi`
  through `q10.mochi`. These currently skip because the compiler returns `0`
  when encountering joins and grouping. Once join support is implemented the
  tests should compile and run the generated C, comparing against the
  `tests/dataset/job` golden outputs.

## Remaining work for TPCH q1-q2

Golden tests now exercise both `q1.mochi` and `q2.mochi` but the generated C
source fails to compile due to missing grouping and join support. Extending
`compileQueryExpr` with general grouping logic and join handling is required so
that these programs build and produce the expected output stored under
`tests/dataset/tpc-h/out`. Once implemented the `tpch_golden_test.go` tests
should compile and run successfully.
