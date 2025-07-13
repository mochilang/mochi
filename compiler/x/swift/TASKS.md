# Swift Compiler Progress

## Recent Enhancements
- 2025-07-13 05:09 – improved type inference for `append()` calls so list variables adopt the element type.
- 2025-07-13 07:03 – fixed nested selector handling and avoided unnecessary casts when grouping by built-in keys.
- 2025-07-13 07:31 – added optional-aware struct generation for join queries.
- 2025-07-13 08:07 – generated initial Swift code for `tpc-h/q1.mochi` but compilation still fails.
- 2025-07-13 08:15 – added support for detecting map fields in group keys and removed
  invalid `@dynamicMemberLookup` extension so query code now compiles.

## Remaining Work
- [ ] Generate safer optional handling in join queries
- [ ] Compile full TPC-H `q1.mochi` without manual adjustments
- [ ] Resolve runtime issues with outer and right joins
