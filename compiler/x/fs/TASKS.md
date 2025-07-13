# F# Compiler Enhancement Notes

## Recent Updates

- 2025-07-13 05:03 - Added simple type inference for identifiers and selectors so
  generated record fields use concrete types.

## Remaining Work

- [ ] Ensure `tpch/q1.mochi` builds by providing the required .NET assemblies.
- [ ] Add coverage tests exercising dataset queries.
- [ ] Continue refining inference to reduce `obj` usage.
