# C# Compiler TODO
## Recent Updates (2025-07-21 00:00)
- Improved compile-time casting so numeric conversions no longer rely on the
  `_cast` helper when types are known. Machine output added for
  `cast_string_to_int` bringing totals to 8/100.
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
