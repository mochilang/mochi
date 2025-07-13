# C# Compiler TODO

## Recent Updates (2025-07-13 09:27)
- Auto-enable `DictMode` only when grouped queries use anonymous structs or maps.
- Regenerated `cross_join` without dictionary indexing.

## Remaining Work
- [ ] Verify dictionary generation for grouped query results across queries.
- [ ] Ensure property ordering matches golden outputs.
- [ ] Compile additional TPC-H queries.
