# Clojure Backend Tasks

The backend now supports membership helpers and formatted code, allowing the
first ten JOB queries to compile and run successfully. Golden tests covering
`q1.mochi` through `q10.mochi` live under `tests/dataset/job/compiler/clj`.

Remaining work:

- Ensure all generated sources pass `cljfmt` without warnings.

## TPCH Queries

The TPCH suite is not yet supported. Compiling `q1.mochi` produces
unformatted code with unmatched parentheses and the generated program
fails to run. Once the compiler can emit valid code, add golden tests
and expected output for `q1.mochi` and `q2.mochi` under
`tests/dataset/tpc-h/compiler/clj`.

With formatting improvements the Clojure backend can now execute the initial JOB
queries without errors.
