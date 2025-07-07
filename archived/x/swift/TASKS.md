# Swift Backend Tasks for TPCH Q1–Q2

The Swift backend can compile the `tpc-h/q1.mochi` benchmark and generates code
for `q2.mochi`. Running the generated Swift with `swiftc` currently fails due to
type errors where dictionary values of type `Any` are compared to integers or
strings.

Completed work:

- `group by` implemented using a helper `_group_by` backed by a dictionary.
- Added numeric helpers `_sum` and `_avg` for arrays.
- Struct values map to `Codable` Swift `struct` types for JSON output.
- Added golden test `tpch_q1.mochi` under `tests/compiler/swift`.
- Added dataset golden test `TestSwiftCompiler_TPCHQ1_Golden` and stored generated code under `tests/dataset/tpc-h/compiler/swift/q1.swift.out`.
- Added `TestSwiftCompiler_TPCHQ2_Golden` and recorded output under `tests/dataset/tpc-h/compiler/swift/q2.swift.out`.
- `_group_by` now hashes keys using JSON for stable grouping.

Further improvements will expand coverage of dataset queries.

- Added JOB dataset golden tests for queries `q1` to `q10` under `tests/compile/x/swift`.
- Generated Swift sources stored as `tests/dataset/job/compiler/swift/q*.swift.out`.
- Running the generated Swift with `swiftc` still fails due to dictionary based
  record handling; future work should emit proper struct types so the programs
  compile.

Recent attempt expanded type inference so map literals with identifier keys
default to `String` and query compilation now tracks loop variable types. The
generated code builds successfully, but Swift compilation of JOB queries fails
when comparing `Any` values (e.g. `ct["id"]! == mc["company_type_id"]!`).
Additional static typing or casts are required before the JOB programs can run
with `swiftc`.

Attempted to compile `tpc-h/q2.mochi` as well. The code is generated but
`swiftc` fails with type errors where map values of type `Any` are compared to
integers or strings. More robust schema generation is needed before the TPCH
programs can run.

Next steps:

- Generate typed struct representations for TPCH tables so dictionary lookups
  are replaced with field access.
- Update code generation to use these structs and ensure the generated Swift
  compiles and runs for `q1` and `q2`.
