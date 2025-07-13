# Clojure Compiler Tasks

Recent updates (2025-07-13 17:53):
- TPCH golden test now runs queries Q1 and Q2 only and checks program output.
- Updated `_query` runtime helper to apply join key functions with `apply` so joins using multiple parameters work.

Remaining work:
- [ ] Fix runtime failure in `outer_join.mochi` due to arity mismatch.
- [ ] Support reading relative files for `load_yaml.mochi` during tests.
- [ ] Expand dataset query coverage beyond TPCH Q1–Q3.
