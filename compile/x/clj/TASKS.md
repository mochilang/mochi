# Clojure Backend Tasks

The backend now supports membership helpers and formatted code, allowing the
first ten JOB queries to compile and run successfully. Golden tests covering
`q1.mochi` through `q10.mochi` live under `tests/dataset/job/compiler/clj`.

Remaining work:

- Ensure all generated sources pass `cljfmt` without warnings.

With formatting improvements the Clojure backend can now execute the initial JOB
queries without errors.
