# COBOL Backend TODO

The current COBOL backend still fails to compile the TPC-H queries. Golden tests
now cover `q1` and `q2` but both are skipped because the generated COBOL does not
build. Query `q1` from the TPC-DS suite is also compiled under
`tests/dataset/tpc-ds/compiler/cobol`, though the resulting program fails to
build for the same reasons. The following work is required to support these queries:

1. **Group by support** – add code generation for `group by` clauses. Queries should accumulate items by key and expose a `Group` structure similar to the Go runtime.
2. **Aggregations** – implement built-in helpers for `sum`, `avg` and `count` that operate over lists and groups.
3. **Dynamic lists** – allow query results to grow at runtime rather than relying on fixed list lengths.
4. ~~**Struct field access** – extend `expr()` to handle nested struct selectors used within queries.~~ (done)
5. **Golden tests** – once the above features compile Q1, add `tpc-h_q1.mochi`, `tpc-h_q1.cob.out` and `tpc-h_q1.out` to `tests/compiler/cobol`.
6. **Runtime helpers** – create small COBOL routines for JSON printing and numeric conversions if needed.

These tasks will bring the COBOL backend closer to running `tests/dataset/tpc-h/q1.mochi` and similar benchmarks.

## JOB Dataset Support

Initial join handling and method call support now allow the COBOL backend to compile the JOB dataset queries `q1.mochi` through `q10.mochi`. Golden tests covering these programs live under `tests/dataset/job/compiler/cobol` and are exercised by `job_test.go`.

### Remaining work

* Implement left/right/outer join semantics.
* Handle group by clauses and additional aggregations.
* Stabilize generated code ordering for JOB queries. The current compiler uses
  maps which lead to nondeterministic join condition ordering. Golden tests set
  `MOCHI_SKIP_COBFMT=1` to avoid formatter installs but output may still vary
  across runs.
