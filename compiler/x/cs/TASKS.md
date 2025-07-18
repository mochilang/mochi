# C# Compiler TODO
## Recent Updates (2025-07-27 12:00)
- Query select map literals now infer struct types, reducing runtime helpers.
- Generated machine output for `fun_call` bringing totals to 32/100.
## Recent Updates (2025-07-27 00:00)
- Map literals are now compiled to structs when their shape is inferrable,
  reducing reliance on dictionary helpers.

## Recent Updates (2025-07-21 12:00)
- Avoid emitting `_cast` when assigning values already typed, using direct
  numeric casts where possible. Machine output added for `binary_precedence`,
  `bool_chain` and `break_continue` bringing totals to 12/100.
## Recent Updates (2025-07-21 00:00)
- Improved compile-time casting so numeric conversions no longer rely on the
  `_cast` helper when types are known. Machine output added for
  `cast_string_to_int` bringing totals to 8/100.
## Recent Updates (2025-07-22 00:00)
- Slicing of typed strings and lists now uses `Substring` and `GetRange`
  instead of helper functions. Generated machine output for `slice`,
  `substring_builtin` and `string_prefix_slice` bringing totals to 12/100.
## Recent Updates (2025-07-20 15:00)
- Added C# machine output for `append_builtin` bringing totals to 6/100.
## Recent Updates (2025-07-19 09:00)
- Verified VM golden tests run with dotnet and regenerated outputs.
- All 100 `tests/vm/valid` programs compile and execute successfully.

## Recent Updates (2025-07-18 12:00)
- Simplified VM golden tests to only verify runtime output and always write
  generated C# files for reference.
- Fixed TPC-DS golden test to use `.cs` not `.cs.out`.

## Recent Updates (2025-07-17 09:00)
- Refactored VM golden tests to strip compiler headers and update README.
- Removed obsolete `compiler_test.go` and `tpch_test.go` duplicates.

## Recent Updates (2025-07-16 15:45)
- Added golden tests for `tests/vm/valid` verifying generated C# code and output.

## Recent Updates (2025-07-13 10:17)
- Auto-enable `DictMode` only when grouped queries use anonymous structs or maps.
- Fixed LINQ compiler to register join variables before compiling query select, removing dictionary indexing in `cross_join`.
- Added direct `Sum` aggregation support for simple queries.
## Recent Updates (2025-07-13 16:52)
- Adjusted group-by code generation to avoid C# generic mismatches.
## Recent Updates (2025-07-15 04:44)
- Added golden test for TPC-DS queries to verify generated code and runtime output.
## Recent Updates (2025-07-15 05:04)
- Began refactoring query codegen to support dynamic grouping and sort keys.
## Recent Updates (2025-07-15 05:46)
- Regenerated TPC-DS q1 golden with dynamic sort fix.
## Recent Updates (2025-07-15 08:17)
  - Expanded TPCH golden test to cover queries `q1`-`q22` and added
  `compile_tpch_cs.go` script to generate golden outputs.
  - Fixed dynamic string comparison in the C# backend.
## Recent Updates (2025-07-16 11:32)
  - Updated Rosetta golden outputs and fixed `len` builtin to return
    `.Count` for lists and maps. This resolves several failed tasks.

## Recent Updates (2025-07-16 17:30)
- Added VM valid golden test harness for C# and fixed map-to-struct casting
  to use literal field names. Removed the `cast_struct.error` artifact.

## Remaining Work
- [ ] Verify dictionary generation for grouped query results across queries.
- [ ] Ensure property ordering matches golden outputs.
- [ ] Compile remaining TPC-H queries.

## Recent Updates (2025-07-19 10:00)
- Added parameter scoping in compileFunStmt so list lengths infer correctly.
- Numeric range loops now type loop variables as ints for better inference.
## Recent Updates (2025-07-23 00:00)
- Extended type inference to drop runtime helpers for indexing and slicing when
  operand types are known. Added machine output for `exists_builtin`,
  `cast_struct`, `closure`, `count_builtin`, `cross_join`,
  `dataset_where_filter`, `string_contains`, `values_builtin`, `group_by`,
  `len_string`, `len_map` and `substring_builtin` bringing totals to 29/100.

## Recent Updates (2025-07-24 00:00)
- Initial attempt to infer struct types from map-based list literals to reduce runtime helpers.
## Recent Updates (2025-07-25 00:00)
- Direct list and map indexing now falls back to variable type hints when
  inference is ambiguous, avoiding `_indexList` and `_indexString` helpers.
  Machine output regenerated for `two-sum`.
## Recent Updates (2025-07-26 00:00)
- Implemented struct inference for map and list literals using new helper functions. Updated compiler to register inferred structs for dataset queries.
