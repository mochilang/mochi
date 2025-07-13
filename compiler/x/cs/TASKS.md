# C# Compiler TODO

## Recent Updates (2025-07-13 10:17)
- Auto-enable `DictMode` only when grouped queries use anonymous structs or maps.
- Fixed LINQ compiler to register join variables before compiling query select, removing dictionary indexing in `cross_join`.
- Added direct `Sum` aggregation support for simple queries.
## Recent Updates (2025-07-13 16:52)
- Adjusted group-by code generation to avoid C# generic mismatches.

## Remaining Work
- [ ] Verify dictionary generation for grouped query results across queries.
- [ ] Ensure property ordering matches golden outputs.
- [ ] Compile remaining TPC-H queries.
