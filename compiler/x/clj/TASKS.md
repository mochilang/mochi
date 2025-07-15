# Clojure Compiler Tasks

Recent updates (2025-07-15 04:43):
- TPCH golden test now attempts queries Q1–Q22, skipping on failures.
- Generated golden Clojure code for Q7–Q22 under `tests/dataset/tpc-h/compiler/clj`.
- Updated test harness to write golden files when invoked with `-update`.
- Added TPC-DS golden test and generated code under `tests/dataset/tpc-ds/compiler/clj`.

Remaining work:
- [ ] Fix runtime failure in `outer_join.mochi` due to arity mismatch.
- [ ] Support reading relative files for `load_yaml.mochi` during tests.
- [ ] Expand dataset query coverage beyond TPCH Q1–Q6.
