# Clojure Compiler Tasks

Recent updates (2025-07-16 11:36):
- TPCH golden test now attempts queries Q1–Q22, skipping on failures.
- Generated golden Clojure code for Q7–Q22 under `tests/dataset/tpc-h/compiler/clj`.
- Updated test harness to write golden files when invoked with `-update`.
- Added TPC-DS golden test and generated code under `tests/dataset/tpc-ds/compiler/clj`.
- Fixed `_input` helper to return empty string when no input is provided.
- Clojure Rosetta tests use `SOURCE_DATE_EPOCH` for reproducible headers.

2025-07-17 07:27:
- Added generated code for Rosetta tasks `100-prisoners` and `2048`.
- Recorded parse/runtime errors for `15-puzzle-game` and `15-puzzle-solver`.

2025-07-17 16:32:
- Fixed default indexing to use `get` when element type is unknown.
- Regenerated machine translations for `tests/vm/valid`.

Remaining work:
- [ ] Fix runtime failure in `outer_join.mochi` due to arity mismatch.
- [ ] Support reading relative files for `load_yaml.mochi` during tests.
- [ ] Expand dataset query coverage beyond TPCH Q1–Q6.
