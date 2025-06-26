# Scheme Backend Tasks for TPCH Q1

The Scheme backend targets chibi-scheme and currently handles only small datasets.

- Represent rows as association lists and group them using hash tables.
- Implement `sum`, `avg` and `count` as SRFI-compatible folds over lists.
- Provide a simple JSON printer and add tests in `tests/compiler/scheme`.
