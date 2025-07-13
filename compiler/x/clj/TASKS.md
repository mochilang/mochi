# Clojure Compiler Tasks

Recent updates (2025-07-13 18:15):
- TPCH golden test expanded to run queries Q1 through Q6.
- Generated golden Clojure code for Q4–Q6.
- Updated `_query` runtime helper to apply join key functions with `apply` so joins using multiple parameters work.

Remaining work:
- [ ] Fix runtime failure in `outer_join.mochi` due to arity mismatch.
- [ ] Support reading relative files for `load_yaml.mochi` during tests.
- [ ] Expand dataset query coverage beyond TPCH Q1–Q6.
