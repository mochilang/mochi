# Clojure Backend Tasks

The backend now supports membership helpers and formatted code, allowing `q1.mochi`
to compile and run successfully. Golden tests have been added for the first ten
queries under `tests/dataset/job/compiler/clj`.

Remaining work:

- Queries `q4.mochi` and `q7.mochi` fail at runtime with `ClassCastException`.
  Investigate incorrect numeric handling in generated code.
- Ensure all generated sources pass `cljfmt` without warnings.

Finishing these items should let the Clojure backend run the first ten JOB
queries without errors.
