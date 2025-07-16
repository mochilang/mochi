# C# Compiler TODO
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

## Remaining Work
- [ ] Verify dictionary generation for grouped query results across queries.
- [ ] Ensure property ordering matches golden outputs.
- [ ] Compile remaining TPC-H queries.
