# C# Compiler TODO

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

## Remaining Work
- [ ] Verify dictionary generation for grouped query results across queries.
- [ ] Ensure property ordering matches golden outputs.
- [ ] Compile remaining TPC-H queries.
