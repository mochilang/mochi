# C# Compiler TODO

## Recent Updates (2025-07-13 09:13)
- Added `DictMode` option to emit dictionaries for anonymous structs.
- Began work on tpc-h q1 support and group query detection.
- Extracted struct literal generation into `compileStructLiteral` for cleaner
  dictionary output.

## Remaining Work
- [ ] Verify dictionary generation for grouped query results across queries.
- [ ] Ensure property ordering matches golden outputs.
- [ ] Run tpc-h q1 end-to-end using `DictMode`.
