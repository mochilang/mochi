# Swift Compiler Progress

## Recent Enhancements
- 2025-07-30 10:00 – outer join queries mark both sides optional during type
  inference so generated structs are reused across statements.
- 2025-07-29 07:00 – optional field checks added and left join placeholders
  use typed optionals so complex joins compile and run.
- 2025-07-28 08:00 – query expressions selecting numeric fields from groups now
  infer element types so `_avg`, `_sum`, `_min` and `_max` helpers are avoided.
- 2025-07-27 09:00 – list element types inferred from struct fields so numeric aggregates drop the `_avg` helper.
- 2025-07-26 12:00 – struct literals now infer their concrete type so `_equal` helper is skipped when comparing typed lists.
- 2025-07-25 09:30 – improved expression type detection for arithmetic chains so direct comparisons avoid `_equal` when possible
- 2025-07-24 09:00 – arithmetic expressions now infer numeric types so expectation comparisons use `==` without the `_equal` helper when possible
- 2025-07-23 10:00 – direct equality for typed values avoids _equal helper when possible
- 2025-07-22 08:10 – improved join query nil handling and added struct list
  conversion when saving data so `right_join` and `save_jsonl_stdout` now
  compile.
- 2025-07-21 07:27 – struct casts no longer track map fields so `cast_struct`
  compiles correctly.
- 2025-07-20 09:00 – numeric literal inference now distinguishes ints and floats so
  join and group queries with decimal fields compile correctly.
- 2025-07-19 12:00 – tuple output for query map literals and identifier keys
  accepted when mapping to structs.
- 2025-07-18 10:00 – simplified golden tests to only check runtime output and
  added summary reporting. Duplicate compile tests were removed.
- 2025-07-17 14:30 – added golden tests for `tests/vm/valid` and switched
  to running code with `swiftc` which reduces failing `.error` files.
- 2025-07-16 13:17 – implemented `now()` built-in to return nanoseconds using
  `Date().timeIntervalSince1970`, allowing more Rosetta tasks to compile.
- 2025-07-17 00:00 – variable declarations with empty lists now scan later
  assignments to infer the element type, avoiding `[Any]`.
- 2025-07-13 05:09 – improved type inference for `append()` calls so list variables adopt the element type.
- 2025-07-13 07:03 – fixed nested selector handling and avoided unnecessary casts when grouping by built-in keys.
- 2025-07-13 07:31 – added optional-aware struct generation for join queries.
- 2025-07-13 08:07 – generated initial Swift code for `tpc-h/q1.mochi` but compilation still fails.
- 2025-07-13 08:15 – added support for detecting map fields in group keys and removed
  invalid `@dynamicMemberLookup` extension so query code now compiles.
- 2025-07-13 09:02 – numeric equality handles Int vs Double so TPC-H `q1` runs correctly.
- 2025-07-13 17:32 – generated code and output for TPC-H `q4`; test now covers queries up to `q6` though later ones still fail.
- 2025-07-15 05:04 – added golden tests for TPC-DS queries and generated Swift
  output for all compilable cases.
- 2025-07-16 11:43 – empty list literals now infer `Any` type only when no
  annotation is provided. Typed variables emit plain `[]` so Swift infers the
  element type from the declaration.

## Remaining Work
- [ ] Generate safer optional handling in join queries
- [x] Compile full TPC-H `q1.mochi` without manual adjustments
- [ ] Resolve runtime issues with outer and right joins
