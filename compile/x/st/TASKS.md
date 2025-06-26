# Smalltalk Backend Tasks for TPCH Q1

The Smalltalk backend emits GNU Smalltalk code but does not yet implement dataset grouping.

- Generate classes for query rows and use `Dictionary` objects to accumulate groups.
- Add methods computing `sum`, `avg` and `count` for each group.
- Print results in JSON using a small helper and update `tests/compiler/st`.
