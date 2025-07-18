# F# Compiler Enhancement Notes

## Recent Updates
- 2025-09-06 - Added option-aware join inference and disabled anonymous struct
  generation during hint collection; 77 of 100 programs compile and run.
- 2025-07-27 00:00 UTC - Improved builtin type inference for common functions and fixed module import syntax.
- 2025-07-27 12:00 UTC - Fixed string `in` operator orientation, corrected group
  bindings formatting, and inferred numeric map key types.

- 2025-07-17 00:00 UTC - Added golden tests for `tests/vm/valid` verifying
  generated F# code and improved type inference on assignments so more
  examples compile without errors.
- 2025-07-21 00:00 UTC - Golden tests now execute F# programs and compare
  runtime output under `tests/machine/x/fs`. Type inference for function
  expressions and cast targets improved so fewer `.error` files are generated.
- 2025-07-17 12:34 UTC - Simplified dataset tests to check only program output
  and corrected TPC-DS output paths.

- 2025-07-16 12:22 UTC - compile script now autodetects `System.Text.Json.dll`
  and `System.Runtime.dll` paths so tests run on machines without exact
  directory versions installed.
- 2025-07-16 15:31 UTC - Compiler tests now remove stale `.error` files and
  autodetect runtime assemblies, reducing spurious failures.
- 2025-07-17 - Map literals now emit `Dictionary` instances for mutation and
  `load` expressions use `let ... in` form; added inference for loaded types.

- 2025-07-16 12:07 UTC - Improved type hints for arrays by analyzing assignments
  and fixed range loop bounds so Rosetta tasks compile and run without errors.

- 2025-07-13 05:03 - Added simple type inference for identifiers and selectors so
  generated record fields use concrete types.
- 2025-07-13 05:19 - Fixed binary expression inference crash by wrapping
  the left-hand side in an `Expr` node.
- 2025-07-16 02:15 UTC - Added Rosetta golden tests and implemented
  `padStart` method calls and `pow` builtin support.

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
- 2025-09-02 - Fixed dictionary membership, improved `print` handling and recursion support, updated join typing. 75 of 100 programs compile and run.
- 2025-09-05 - Corrected string expression detection so boolean string comparisons print with `%b`. 76 of 100 programs compile and run.
- 2025-11-23 - Parenthesized function call arguments and combined return branches so recursive examples like `tail_recursion` compile; 79 of 100 programs now succeed.
- 2025-11-24 - Improved list print handling and map value type inference, removing extra `List.map string` calls when element types are known. 80 of 100 programs compile and run.
- 2025-12-01 - Inlined set operations and fixed import indentation; `go_auto` now compiles. 81 of 100 programs compile and run.
- 2025-12-05 - Method-call inference updated so `string_contains` compiles without helper functions; 82 of 100 programs compile and run.
- 2025-12-10 - Added nested selector inference and formatted print strings; `right_join` now compiles. 83 of 100 programs compile and run.
- 2025-12-20 - Removed '=' from module headers in import generation so Python math modules compile; 85 of 100 programs compile and run.
