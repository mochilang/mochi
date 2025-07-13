# Java Compiler Tasks

## Recent enhancements
- 2025-07-13 05:05 - improved `dataClassFor` to refine field types on repeated compilation.
- 2025-07-13 05:26 - added wildcard generics to reduce reliance on `Object` types.
- 2025-07-13 07:59 - direct list mutation for standalone `append` calls.
- 2025-07-13 15:40 - added substring type inference for grouping fields.

## Remaining work
- [ ] Improve query loop readability
- [ ] Trim remaining uses of helper methods for builtins
- [ ] Investigate performance of TPCH queries on larger datasets
- [ ] Support streaming JSON output for large datasets
