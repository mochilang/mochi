# PHP Backend Tasks for TPCH Q1

The PHP backend now supports dataset queries used by `tpc-h/q1.mochi`.

- Implemented `sum` helper alongside `avg` and `count` using PHP arrays.
- Added support for `group by` with an optional `where` clause.
- Records are represented as associative arrays and output uses `json_encode`.
- Golden tests live under `tests/compiler/php`.
