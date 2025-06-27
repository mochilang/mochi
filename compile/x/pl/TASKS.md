# Prolog Backend Tasks for TPCH Q1

Support for TPCH Q1 has been implemented using helper predicates for grouping
and aggregation. The backend now provides:

- `sum/2`, `avg/2` and `count/2` predicates working on lists and groups.
- `json/1` which prints values using `json_write_dict/2` from the HTTP library.
- Golden tests covering the query under `tests/compiler/pl` and
  `tests/dataset/tpc-h/compiler/pl`.

Additional optimisations may be explored but the example now compiles and runs.

## TPCH Queries Q1-Q2

Initial work allowed compiling the first TPCH query but the generated code does
not execute correctly because filter lambdas are emitted as two-argument
predicates. The new golden tests compile both `q1.mochi` and `q2.mochi`. Query
`q2` runs successfully, however `q1` still fails at runtime. Remaining work is
to adapt `dataset_filter/3` to call predicates that succeed or fail without
extra result parameters so that `q1` executes.

## JOB Queries Q1-Q10

Initial support was added for compiling JOB queries q1 through q10.  The
compiler generates Prolog source successfully, however running the programs
revealed missing runtime predicates and language features.  Examples include
`starts_with/3` and other string helpers used by several queries.  Future work
should implement these helpers and ensure all queries execute correctly.
