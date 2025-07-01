# PHP Backend Dataset Queries

The PHP backend now supports dataset queries used by `tpc-h/q1.mochi`, `tpc-h/q2.mochi`,
`tpc-ds/q1.mochi` and `job/q1.mochi` through `job/q10.mochi`.

- Implemented `sum` helper alongside `avg` and `count` using PHP arrays.
- Added support for `group by` with an optional `where` clause.
- Added `json` helper which prints `json_encode` output.
- Fixed runtime emission for `_Group` when grouping.
- Records are represented as associative arrays and output uses `json_encode`.
- Selectors on map values now emit array indexing syntax. Unknown variables are
  assumed to be maps so dataset rows work without explicit types.
- The string/list `contains` method is translated to the `in` operator logic.
- Golden tests live under `tests/compiler/php` and cover both TPCH and JOB examples.
  TPCH queries `q1` and `q2` have compiled PHP output and runtime results checked
  in under `tests/dataset/tpc-h/compiler/php`. The first TPCDS query `q1` is
  available under `tests/dataset/tpc-ds/compiler/php`.

## Remaining work

- Extend support to remaining JOB queries beyond `q10`.
