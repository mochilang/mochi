# F# Compiler Enhancement Notes

## Recent Updates

- 2025-07-13 05:03 - Added simple type inference for identifiers and selectors so
  generated record fields use concrete types.
- 2025-07-13 05:19 - Fixed binary expression inference crash by wrapping
  the left-hand side in an `Expr` node.

## Remaining Work

- [ ] Ensure `tpch/q1.mochi` builds by providing the required .NET assemblies.
- [ ] Generate F# code for `tpch/q1.mochi` during tests.
- [ ] Add coverage tests exercising dataset queries.
- [ ] Continue refining inference to reduce `obj` usage.

- 2025-07-14 00:20 - Installed mono and dotnet runtime. q1 still fails to compile due to type errors in generated code; investigate group_by and numeric inference.
- 2025-07-15 04:48 - Generated F# code for all TPC-DS queries using new script. Added
  initial `tpcds_test.go` but compilation of generated code fails with numerous
  type errors.


- 2025-07-15 06:37 - Implemented tuple-based sort key generation in `compileQuery` to allow sorting before dropping query variables. F# code for TPC-DS queries still fails to compile due to other type issues.
- 2025-07-15 07:18 - Added `compile_tpcds_fs.go` to regenerate F# code and outputs for all TPC-DS queries. Compilation still fails during tests despite dotnet runtime installation.
