# Clojure Backend Tasks

The current backend can parse simple programs but JOB dataset queries still fail to compile. 
To support the JOB benchmark we need several improvements:

- Implement string and list membership helpers. `x.contains(y)` should translate to
  `clojure.string/includes?` for strings and use the `_in` helper for lists and maps.
- Validate generated code to ensure balanced parentheses; running cljfmt currently
  reports "EOF while reading" for q1.
- Extend the JOB golden tests to compile and execute `q1.mochi` through `q10.mochi`.
  Generated sources should live under `tests/dataset/job/compiler/clj`.
- Emit correct JSON output so the tests can compare results.

Once these features are complete the Clojure backend should be able to run the
first ten JOB queries.
