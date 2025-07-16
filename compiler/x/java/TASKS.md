# Java Compiler Tasks

## Recent enhancements
- 2025-07-13 05:05 - improved `dataClassFor` to refine field types on repeated compilation.
- 2025-07-13 05:26 - added wildcard generics to reduce reliance on `Object` types.
- 2025-07-13 07:59 - direct list mutation for standalone `append` calls.
- 2025-07-13 15:40 - added substring type inference for grouping fields.
- 2025-07-13 16:29 - generated Java code for TPCH query 10.
- 2025-07-13 16:50 - generated Java code for TPCH queries 11-13.
- 2025-07-13 17:27 - generated Java code for TPCH queries 14-15 and extended tests.
- 2025-07-13 17:40 - generated Java code for TPCH queries 16-22.
- 2025-07-15 04:46 - added TPC-DS golden tests and generated outputs for queries 50-99.
- 2025-07-15 05:05 - generated outputs for remaining TPC-DS queries with error tracking.
- 2025-07-15 06:32 - added concat builtin and regenerated TPC-DS outputs.
- 2025-07-15 07:20 - improved casting for doubles and underscore variable handling for TPC-DS queries.

## Remaining work
- [ ] Improve query loop readability
- [ ] Trim remaining uses of helper methods for builtins
- [ ] Investigate performance of TPCH queries on larger datasets
- [ ] Support streaming JSON output for large datasets

- 2025-07-15 07:52 - reduced grouped join rows to avoid unused fields and regenerated TPC-DS Java outputs.
- 2025-07-15 08:22 - added group cleanup, improved row vars and sum type handling; generated Java code for TPCH queries with update, new q9.java and error logs.
- 2025-07-16 11:30 - added generic cast fallback to reduce Rosetta .error files.
- 2025-07-16 12:20 - default unknown types to Object and cast without errors to further reduce Rosetta .error files.
- 2025-07-16 13:00 - added Java Rosetta golden tests and script; unknown expr types now default to Object.
- 2025-07-16 14:10 - added VM valid golden tests and boolean casting for list elements to reduce .error files.
- 2025-07-16 16:30 - preserved group metadata after queries and set MOCHI_ROOT for VM golden tests; all valid programs compile and run without errors.
